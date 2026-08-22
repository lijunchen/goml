package main

import (
    _goml_fmt "fmt"
    _goml_reflect "reflect"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec__goml_m_std_p_channel_p_Operation____isize struct {
    items []_goml_m_std_p_channel_p_Operation____isize
}

func vec_new___goml_m_Vec__30std_p_channel_p_Operation____isize() *_goml_vec__goml_m_std_p_channel_p_Operation____isize {
    return &_goml_vec__goml_m_std_p_channel_p_Operation____isize{
        items: nil,
    }
}

func vec_len___goml_m_Vec__30std_p_channel_p_Operation____isize(vec *_goml_vec__goml_m_std_p_channel_p_Operation____isize) int {
    return int(len(vec.items))
}

type _goml_vec_int struct {
    items []int
}

func vec_new__Vec_3int() *_goml_vec_int {
    return &_goml_vec_int{
        items: nil,
    }
}

func vec_with_capacity__Vec_3int(capacity int) *_goml_vec_int {
    return &_goml_vec_int{
        items: make([]int, 0, capacity),
    }
}

func vec_push__Vec_3int(vec *_goml_vec_int, elem int) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_len__Vec_3int(vec *_goml_vec_int) int {
    return int(len(vec.items))
}

type _goml_vec_Receiver_3int struct {
    items []<-chan int
}

func vec_new__Vec_13Receiver_3int() *_goml_vec_Receiver_3int {
    return &_goml_vec_Receiver_3int{
        items: nil,
    }
}

func vec_push__Vec_13Receiver_3int(vec *_goml_vec_Receiver_3int, elem <-chan int) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_len__Vec_13Receiver_3int(vec *_goml_vec_Receiver_3int) int {
    return int(len(vec.items))
}

type _goml_vec_Sender_3int struct {
    items []chan<- int
}

func vec_new__Vec_11Sender_3int() *_goml_vec_Sender_3int {
    return &_goml_vec_Sender_3int{
        items: nil,
    }
}

func vec_push__Vec_11Sender_3int(vec *_goml_vec_Sender_3int, elem chan<- int) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_len__Vec_11Sender_3int(vec *_goml_vec_Sender_3int) int {
    return int(len(vec.items))
}

type Tuple4_10Slice_3int_21Slice_13Receiver_3int_19Slice_11Sender_3int_10Slice_3int struct {
    _0 []int
    _1 []<-chan int
    _2 []chan<- int
    _3 []int
}

type Tuple5_3int_3int_3int_4bool_4bool struct {
    _0 int
    _1 int
    _2 int
    _3 bool
    _4 bool
}

type Tuple2_3int_4bool struct {
    _0 int
    _1 bool
}

type _goml_m_std_p_channel_p_PreparedOperations____isize struct {
    kinds *_goml_vec_int
    receivers *_goml_vec_Receiver_3int
    senders *_goml_vec_Sender_3int
    values *_goml_vec_int
}

type Ordering int32

type _goml_m_std_p_channel_p_SelectError int32

const (
    Empty _goml_m_std_p_channel_p_SelectError = 0
)

type _goml_m_std_p_channel_p_Selection____isize interface {
    is_goml_m_std_p_channel_p_Selection____isize()
}

type Received struct {
    _0 int
    _1 Option__isize
}

func (_ Received) is_goml_m_std_p_channel_p_Selection____isize() {}

type Sent struct {
    _0 int
}

func (_ Sent) is_goml_m_std_p_channel_p_Selection____isize() {}

type Option__isize struct {
    _tag int32
    _v1_0 int
}

type _goml_m_std_p_channel_p_Operation____isize struct {
    _tag int32
    _v0_0 <-chan int
    _v1_0 chan<- int
    _v1_1 int
}

type _goml_m_Option____std_p_channel_p_Selection____isize struct {
    _tag int32
    _v1_0 _goml_m_std_p_channel_p_Selection____isize
}

type _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError struct {
    _tag int32
    _v0_0 _goml_m_Option____std_p_channel_p_Selection____isize
    _v1_0 _goml_m_std_p_channel_p_SelectError
}

type _goml_m_Result____std_p_channe_h5400d2fb6c865f5b36aaa9b0f9e787f7_l_p_SelectError struct {
    _tag int32
    _v0_0 _goml_m_std_p_channel_p_Selection____isize
    _v1_0 _goml_m_std_p_channel_p_SelectError
}

func print_selection(value__0 _goml_m_std_p_channel_p_Selection____isize) struct{} {
    switch value__0.(type) {
    case Received:
        var x411 int = value__0.(Received)._0
        var x412 Option__isize = value__0.(Received)._1
        var inline636 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(x411)
        _goml_runtime_core_string_println(inline636)
        var t442 int
        var inline632 int = -1
        switch x412._tag {
        case 0:
            t442 = inline632
        case 1:
            var inline633 int = x412._v1_0
            t442 = inline633
        default:
            panic("non-exhaustive match")
        }
        var inline629 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t442)
        _goml_runtime_core_string_println(inline629)
        return struct{}{}
    case Sent:
        var x413 int = value__0.(Sent)._0
        var inline639 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(x413)
        _goml_runtime_core_string_println(inline639)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var first__4 chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__isize(1)
    var second__5 chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__isize(1)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__isize(first__4, 10)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__isize(second__5, 20)
    var t446 <-chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_receiver____T__isize(first__4)
    var t447 _goml_m_std_p_channel_p_Operation____isize = _goml_m_std_p_channel_p_Operation____isize{
        _tag: 0,
        _v0_0: t446,
    }
    var t448 <-chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_receiver____T__isize(second__5)
    var t449 _goml_m_std_p_channel_p_Operation____isize = _goml_m_std_p_channel_p_Operation____isize{
        _tag: 0,
        _v0_0: t448,
    }
    var t450 [2]_goml_m_std_p_channel_p_Operation____isize = [2]_goml_m_std_p_channel_p_Operation____isize{t447, t449}
    var operations__6 *_goml_vec__goml_m_std_p_channel_p_Operation____isize = func(values [2]_goml_m_std_p_channel_p_Operation____isize) *_goml_vec__goml_m_std_p_channel_p_Operation____isize {
        var storage struct {
            vector _goml_vec__goml_m_std_p_channel_p_Operation____isize
            values [2]_goml_m_std_p_channel_p_Operation____isize
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t450)
    var t451 int = _goml_m_inherent_i_Vec_i_Vec_l_h6161506801e1e3b7949d66d7b76c6b61_tion_l_isize_r_(operations__6)
    var t452 []_goml_m_std_p_channel_p_Operation____isize = operations__6.items[0:t451]
    var mtmp417 _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError = _goml_m_std_p_channel_p_try__select__priority____T__isize(t452)
    switch mtmp417._tag {
    case 0:
        var x418 _goml_m_Option____std_p_channel_p_Selection____isize = mtmp417._v0_0
        switch x418._tag {
        case 1:
            var x420 _goml_m_std_p_channel_p_Selection____isize = x418._v1_0
            switch x420.(type) {
            case Received:
                var inline642 int = x420.(Received)._0
                var inline643 Option__isize = x420.(Received)._1
                println__T_isize(inline642)
                var inline647 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__isize(inline643, -1)
                println__T_isize(inline647)
            case Sent:
                var inline649 int = x420.(Sent)._0
                println__T_isize(inline649)
            default:
                panic("non-exhaustive match")
            }
        default:
            var inline653 int = -1
            var inline654 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline653)
            _goml_runtime_core_string_println(inline654)
        }
    default:
        var inline657 int = -1
        var inline658 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline657)
        _goml_runtime_core_string_println(inline658)
    }
    var t454 int = _goml_m_inherent_i_Vec_i_Vec_l_h6161506801e1e3b7949d66d7b76c6b61_tion_l_isize_r_(operations__6)
    var t455 []_goml_m_std_p_channel_p_Operation____isize = operations__6.items[0:t454]
    var mtmp422 _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError = _goml_m_std_p_channel_p_try__select____T__isize(t455)
    switch mtmp422._tag {
    case 0:
        var x423 _goml_m_Option____std_p_channel_p_Selection____isize = mtmp422._v0_0
        switch x423._tag {
        case 1:
            var x425 _goml_m_std_p_channel_p_Selection____isize = x423._v1_0
            switch x425.(type) {
            case Received:
                var inline661 int = x425.(Received)._0
                var inline662 Option__isize = x425.(Received)._1
                println__T_isize(inline661)
                var inline666 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__isize(inline662, -1)
                println__T_isize(inline666)
            case Sent:
                var inline668 int = x425.(Sent)._0
                println__T_isize(inline668)
            default:
                panic("non-exhaustive match")
            }
        default:
            var inline672 int = -2
            var inline673 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline672)
            _goml_runtime_core_string_println(inline673)
        }
    default:
        var inline676 int = -2
        var inline677 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline676)
        _goml_runtime_core_string_println(inline677)
    }
    var empty__9 chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__isize(0)
    var target__10 chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__isize(1)
    var t457 <-chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_receiver____T__isize(empty__9)
    var t458 _goml_m_std_p_channel_p_Operation____isize = _goml_m_std_p_channel_p_Operation____isize{
        _tag: 0,
        _v0_0: t457,
    }
    var t459 chan<- int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_sender____T__isize(target__10)
    var t460 _goml_m_std_p_channel_p_Operation____isize = _goml_m_std_p_channel_p_Operation____isize{
        _tag: 1,
        _v1_0: t459,
        _v1_1: 30,
    }
    var t461 [2]_goml_m_std_p_channel_p_Operation____isize = [2]_goml_m_std_p_channel_p_Operation____isize{t458, t460}
    var mixed__11 *_goml_vec__goml_m_std_p_channel_p_Operation____isize = func(values [2]_goml_m_std_p_channel_p_Operation____isize) *_goml_vec__goml_m_std_p_channel_p_Operation____isize {
        var storage struct {
            vector _goml_vec__goml_m_std_p_channel_p_Operation____isize
            values [2]_goml_m_std_p_channel_p_Operation____isize
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t461)
    var t462 int = _goml_m_inherent_i_Vec_i_Vec_l_h6161506801e1e3b7949d66d7b76c6b61_tion_l_isize_r_(mixed__11)
    var t463 []_goml_m_std_p_channel_p_Operation____isize = mixed__11.items[0:t462]
    var mtmp427 _goml_m_Result____std_p_channe_h5400d2fb6c865f5b36aaa9b0f9e787f7_l_p_SelectError = _goml_m_std_p_channel_p_select____T__isize(t463)
    switch mtmp427._tag {
    case 0:
        var x428 _goml_m_std_p_channel_p_Selection____isize = mtmp427._v0_0
        print_selection(x428)
    case 1:
        var inline680 int = -3
        var inline681 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline680)
        _goml_runtime_core_string_println(inline681)
    default:
        panic("non-exhaustive match")
    }
    var t465 Option__isize = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__isize(target__10)
    var t466 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__isize(t465, -4)
    println__T_isize(t466)
    var t467 <-chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_receiver____T__isize(empty__9)
    var t468 _goml_m_std_p_channel_p_Operation____isize = _goml_m_std_p_channel_p_Operation____isize{
        _tag: 0,
        _v0_0: t467,
    }
    var t469 [1]_goml_m_std_p_channel_p_Operation____isize = [1]_goml_m_std_p_channel_p_Operation____isize{t468}
    var none__13 *_goml_vec__goml_m_std_p_channel_p_Operation____isize = func(values [1]_goml_m_std_p_channel_p_Operation____isize) *_goml_vec__goml_m_std_p_channel_p_Operation____isize {
        var storage struct {
            vector _goml_vec__goml_m_std_p_channel_p_Operation____isize
            values [1]_goml_m_std_p_channel_p_Operation____isize
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t469)
    var t470 int = _goml_m_inherent_i_Vec_i_Vec_l_h6161506801e1e3b7949d66d7b76c6b61_tion_l_isize_r_(none__13)
    var t471 []_goml_m_std_p_channel_p_Operation____isize = none__13.items[0:t470]
    var mtmp432 _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError = _goml_m_std_p_channel_p_try__select____T__isize(t471)
    var jp473 int
    switch mtmp432._tag {
    case 0:
        var x433 _goml_m_Option____std_p_channel_p_Selection____isize = mtmp432._v0_0
        switch x433._tag {
        case 0:
            jp473 = 40
        default:
            jp473 = -5
        }
    default:
        jp473 = -5
    }
    println__T_isize(jp473)
    var no_operations__14 *_goml_vec__goml_m_std_p_channel_p_Operation____isize
    var inline689 *_goml_vec__goml_m_std_p_channel_p_Operation____isize = vec_new___goml_m_Vec__30std_p_channel_p_Operation____isize()
    no_operations__14 = inline689
    var t474 int
    var inline687 int = vec_len___goml_m_Vec__30std_p_channel_p_Operation____isize(no_operations__14)
    t474 = inline687
    var t475 []_goml_m_std_p_channel_p_Operation____isize = no_operations__14.items[0:t474]
    var mtmp437 _goml_m_Result____std_p_channe_h5400d2fb6c865f5b36aaa9b0f9e787f7_l_p_SelectError = _goml_m_std_p_channel_p_select____T__isize(t475)
    var jp477 int
    switch mtmp437._tag {
    case 1:
        var x439 _goml_m_std_p_channel_p_SelectError = mtmp437._v1_0
        switch x439 {
        case Empty:
            jp477 = 50
        default:
            panic("non-exhaustive match")
        }
    default:
        jp477 = -6
    }
    var inline684 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(jp477)
    _goml_runtime_core_string_println(inline684)
    return struct{}{}
}

func println__T_isize(value__1 int) struct{} {
    var t494 string
    var inline691 string = _goml_runtime_core_int_to_string(value__1)
    t494 = inline691
    _goml_runtime_core_string_println(t494)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__isize(self__467 Option__isize, fallback__468 int) int {
    switch self__467._tag {
    case 0:
        return fallback__468
    case 1:
        var x390 int = self__467._v1_0
        return x390
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__isize(capacity__435 int) chan int {
    var t502 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(capacity__435)
    return t502
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__isize(self__436 chan int, value__437 int) struct{} {
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(self__436, value__437)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_receiver____T__isize(self__443 chan int) <-chan int {
    var t507 <-chan int = func(p0 chan int) <-chan int {
        return p0
    }(self__443)
    return t507
}

func _goml_m_std_p_channel_p_try__select__priority____T__isize(operations__35 []_goml_m_std_p_channel_p_Operation____isize) _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError {
    var t512 int
    var inline700 int = len(operations__35)
    t512 = inline700
    var t513 bool = t512 == 0
    if t513 {
        var t514 _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError = _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError{
            _tag: 1,
            _v1_0: Empty,
        }
        return t514
    } else {
        var prepared__36 _goml_m_std_p_channel_p_PreparedOperations____isize = _goml_m_std_p_channel_p_prepare____T__isize(operations__35)
        var mtmp34 Tuple4_10Slice_3int_21Slice_13Receiver_3int_19Slice_11Sender_3int_10Slice_3int = _goml_m_std_p_channel_p_prepared__slices____T__isize(prepared__36)
        var x35 []int = mtmp34._0
        var x36 []<-chan int = mtmp34._1
        var x37 []chan<- int = mtmp34._2
        var x38 []int = mtmp34._3
        var mtmp39 Tuple5_3int_3int_3int_4bool_4bool = func(p0 []int, p1 []<-chan int, p2 []chan<- int, p3 []int) Tuple5_3int_3int_3int_4bool_4bool {
            var index int = 0
            var receive_index int = 0
            var send_index int = 0
            var value int
            var open bool
            var selected bool
            var cases []_goml_reflect.SelectCase = make([]_goml_reflect.SelectCase, 2)
            cases[1] = _goml_reflect.SelectCase{
                Dir: _goml_reflect.SelectDefault,
            }
            var chosen int
            var received _goml_reflect.Value
            for {
                if index >= len(p0) {
                    break
                }
                if p0[index] == 0 {
                    cases[0] = _goml_reflect.SelectCase{
                        Dir: _goml_reflect.SelectRecv,
                        Chan: _goml_reflect.ValueOf(p1[receive_index]),
                    }
                    chosen, received, open = _goml_reflect.Select(cases)
                    if chosen == 0 {
                        value = received.Interface().(int)
                        selected = true
                    }
                    receive_index = receive_index + 1
                } else {
                    cases[0] = _goml_reflect.SelectCase{
                        Dir: _goml_reflect.SelectSend,
                        Chan: _goml_reflect.ValueOf(p2[send_index]),
                        Send: _goml_reflect.ValueOf(p3[send_index]),
                    }
                    chosen, received, open = _goml_reflect.Select(cases)
                    if chosen == 0 {
                        selected = true
                    }
                    send_index = send_index + 1
                }
                if selected {
                    return Tuple5_3int_3int_3int_4bool_4bool{
                        _0: index,
                        _1: p0[index],
                        _2: value,
                        _3: open,
                        _4: true,
                    }
                }
                index = index + 1
            }
            return Tuple5_3int_3int_3int_4bool_4bool{
                _0: -1,
                _1: -1,
                _2: value,
                _3: false,
                _4: false,
            }
        }(x35, x36, x37, x38)
        var x40 int = mtmp39._0
        var x41 int = mtmp39._1
        var x42 int = mtmp39._2
        var x43 bool = mtmp39._3
        var x44 bool = mtmp39._4
        var jp516 _goml_m_Option____std_p_channel_p_Selection____isize
        if x44 {
            var t518 _goml_m_std_p_channel_p_Selection____isize
            var inline693 bool = x41 == 0
            if inline693 {
                var inline695 Option__isize
                if x43 {
                    var inline697 Option__isize = Option__isize{
                        _tag: 1,
                        _v1_0: x42,
                    }
                    inline695 = inline697
                } else {
                    inline695 = Option__isize{
                        _tag: 0,
                    }
                }
                var inline696 _goml_m_std_p_channel_p_Selection____isize = Received{
                    _0: x40,
                    _1: inline695,
                }
                t518 = inline696
            } else {
                var inline698 _goml_m_std_p_channel_p_Selection____isize = Sent{
                    _0: x40,
                }
                t518 = inline698
            }
            var t519 _goml_m_Option____std_p_channel_p_Selection____isize = _goml_m_Option____std_p_channel_p_Selection____isize{
                _tag: 1,
                _v1_0: t518,
            }
            jp516 = t519
        } else {
            jp516 = _goml_m_Option____std_p_channel_p_Selection____isize{
                _tag: 0,
            }
        }
        var t517 _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError = _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError{
            _tag: 0,
            _v0_0: jp516,
        }
        return t517
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_h6161506801e1e3b7949d66d7b76c6b61_tion_l_isize_r_(self__273 *_goml_vec__goml_m_std_p_channel_p_Operation____isize) int {
    var t522 int = vec_len___goml_m_Vec__30std_p_channel_p_Operation____isize(self__273)
    return t522
}

func _goml_m_std_p_channel_p_try__select____T__isize(operations__24 []_goml_m_std_p_channel_p_Operation____isize) _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError {
    var t527 int
    var inline709 int = len(operations__24)
    t527 = inline709
    var t528 bool = t527 == 0
    if t528 {
        var t529 _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError = _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError{
            _tag: 1,
            _v1_0: Empty,
        }
        return t529
    } else {
        var prepared__25 _goml_m_std_p_channel_p_PreparedOperations____isize = _goml_m_std_p_channel_p_prepare____T__isize(operations__24)
        var mtmp23 Tuple4_10Slice_3int_21Slice_13Receiver_3int_19Slice_11Sender_3int_10Slice_3int = _goml_m_std_p_channel_p_prepared__slices____T__isize(prepared__25)
        var x24 []int = mtmp23._0
        var x25 []<-chan int = mtmp23._1
        var x26 []chan<- int = mtmp23._2
        var x27 []int = mtmp23._3
        var mtmp28 Tuple5_3int_3int_3int_4bool_4bool = func(p0 []int, p1 []<-chan int, p2 []chan<- int, p3 []int) Tuple5_3int_3int_3int_4bool_4bool {
            var cases []_goml_reflect.SelectCase = make([]_goml_reflect.SelectCase, len(p0) + 1)
            var index int = 0
            var receive_index int = 0
            var send_index int = 0
            for {
                if index >= len(p0) {
                    break
                }
                if p0[index] == 0 {
                    cases[index] = _goml_reflect.SelectCase{
                        Dir: _goml_reflect.SelectRecv,
                        Chan: _goml_reflect.ValueOf(p1[receive_index]),
                    }
                    receive_index = receive_index + 1
                } else {
                    cases[index] = _goml_reflect.SelectCase{
                        Dir: _goml_reflect.SelectSend,
                        Chan: _goml_reflect.ValueOf(p2[send_index]),
                        Send: _goml_reflect.ValueOf(p3[send_index]),
                    }
                    send_index = send_index + 1
                }
                index = index + 1
            }
            cases[len(p0)] = _goml_reflect.SelectCase{
                Dir: _goml_reflect.SelectDefault,
            }
            var chosen int
            var received _goml_reflect.Value
            var open bool
            chosen, received, open = _goml_reflect.Select(cases)
            var value int
            if chosen == len(p0) {
                return Tuple5_3int_3int_3int_4bool_4bool{
                    _0: -1,
                    _1: -1,
                    _2: value,
                    _3: false,
                    _4: false,
                }
            }
            if p0[chosen] == 0 {
                value = received.Interface().(int)
            }
            return Tuple5_3int_3int_3int_4bool_4bool{
                _0: chosen,
                _1: p0[chosen],
                _2: value,
                _3: open,
                _4: true,
            }
        }(x24, x25, x26, x27)
        var x29 int = mtmp28._0
        var x30 int = mtmp28._1
        var x31 int = mtmp28._2
        var x32 bool = mtmp28._3
        var x33 bool = mtmp28._4
        var jp531 _goml_m_Option____std_p_channel_p_Selection____isize
        if x33 {
            var t533 _goml_m_std_p_channel_p_Selection____isize
            var inline702 bool = x30 == 0
            if inline702 {
                var inline704 Option__isize
                if x32 {
                    var inline706 Option__isize = Option__isize{
                        _tag: 1,
                        _v1_0: x31,
                    }
                    inline704 = inline706
                } else {
                    inline704 = Option__isize{
                        _tag: 0,
                    }
                }
                var inline705 _goml_m_std_p_channel_p_Selection____isize = Received{
                    _0: x29,
                    _1: inline704,
                }
                t533 = inline705
            } else {
                var inline707 _goml_m_std_p_channel_p_Selection____isize = Sent{
                    _0: x29,
                }
                t533 = inline707
            }
            var t534 _goml_m_Option____std_p_channel_p_Selection____isize = _goml_m_Option____std_p_channel_p_Selection____isize{
                _tag: 1,
                _v1_0: t533,
            }
            jp531 = t534
        } else {
            jp531 = _goml_m_Option____std_p_channel_p_Selection____isize{
                _tag: 0,
            }
        }
        var t532 _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError = _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError{
            _tag: 0,
            _v0_0: jp531,
        }
        return t532
    }
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_sender____T__isize(self__442 chan int) chan<- int {
    var t537 chan<- int = func(p0 chan int) chan<- int {
        return p0
    }(self__442)
    return t537
}

func _goml_m_std_p_channel_p_select____T__isize(operations__14 []_goml_m_std_p_channel_p_Operation____isize) _goml_m_Result____std_p_channe_h5400d2fb6c865f5b36aaa9b0f9e787f7_l_p_SelectError {
    var t542 int
    var inline718 int = len(operations__14)
    t542 = inline718
    var t543 bool = t542 == 0
    if t543 {
        var t544 _goml_m_Result____std_p_channe_h5400d2fb6c865f5b36aaa9b0f9e787f7_l_p_SelectError = _goml_m_Result____std_p_channe_h5400d2fb6c865f5b36aaa9b0f9e787f7_l_p_SelectError{
            _tag: 1,
            _v1_0: Empty,
        }
        return t544
    } else {
        var prepared__15 _goml_m_std_p_channel_p_PreparedOperations____isize = _goml_m_std_p_channel_p_prepare____T__isize(operations__14)
        var mtmp12 Tuple4_10Slice_3int_21Slice_13Receiver_3int_19Slice_11Sender_3int_10Slice_3int = _goml_m_std_p_channel_p_prepared__slices____T__isize(prepared__15)
        var x13 []int = mtmp12._0
        var x14 []<-chan int = mtmp12._1
        var x15 []chan<- int = mtmp12._2
        var x16 []int = mtmp12._3
        var mtmp17 Tuple5_3int_3int_3int_4bool_4bool = func(p0 []int, p1 []<-chan int, p2 []chan<- int, p3 []int) Tuple5_3int_3int_3int_4bool_4bool {
            var cases []_goml_reflect.SelectCase = make([]_goml_reflect.SelectCase, len(p0))
            var index int = 0
            var receive_index int = 0
            var send_index int = 0
            for {
                if index >= len(p0) {
                    break
                }
                if p0[index] == 0 {
                    cases[index] = _goml_reflect.SelectCase{
                        Dir: _goml_reflect.SelectRecv,
                        Chan: _goml_reflect.ValueOf(p1[receive_index]),
                    }
                    receive_index = receive_index + 1
                } else {
                    cases[index] = _goml_reflect.SelectCase{
                        Dir: _goml_reflect.SelectSend,
                        Chan: _goml_reflect.ValueOf(p2[send_index]),
                        Send: _goml_reflect.ValueOf(p3[send_index]),
                    }
                    send_index = send_index + 1
                }
                index = index + 1
            }
            var chosen int
            var received _goml_reflect.Value
            var open bool
            chosen, received, open = _goml_reflect.Select(cases)
            var value int
            if p0[chosen] == 0 {
                value = received.Interface().(int)
            }
            return Tuple5_3int_3int_3int_4bool_4bool{
                _0: chosen,
                _1: p0[chosen],
                _2: value,
                _3: open,
                _4: true,
            }
        }(x13, x14, x15, x16)
        var x18 int = mtmp17._0
        var x19 int = mtmp17._1
        var x20 int = mtmp17._2
        var x21 bool = mtmp17._3
        var t545 _goml_m_std_p_channel_p_Selection____isize
        var inline711 bool = x19 == 0
        if inline711 {
            var inline713 Option__isize
            if x21 {
                var inline715 Option__isize = Option__isize{
                    _tag: 1,
                    _v1_0: x20,
                }
                inline713 = inline715
            } else {
                inline713 = Option__isize{
                    _tag: 0,
                }
            }
            var inline714 _goml_m_std_p_channel_p_Selection____isize = Received{
                _0: x18,
                _1: inline713,
            }
            t545 = inline714
        } else {
            var inline716 _goml_m_std_p_channel_p_Selection____isize = Sent{
                _0: x18,
            }
            t545 = inline716
        }
        var t546 _goml_m_Result____std_p_channe_h5400d2fb6c865f5b36aaa9b0f9e787f7_l_p_SelectError = _goml_m_Result____std_p_channe_h5400d2fb6c865f5b36aaa9b0f9e787f7_l_p_SelectError{
            _tag: 0,
            _v0_0: t545,
        }
        return t546
    }
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__isize(self__438 chan int) Option__isize {
    var mtmp379 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(self__438)
    var x380 int = mtmp379._0
    var x381 bool = mtmp379._1
    if x381 {
        var t551 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: x380,
        }
        return t551
    } else {
        return Option__isize{
            _tag: 0,
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t557 string = _goml_runtime_core_int_to_string(self__151)
    return t557
}

func _goml_m_std_p_channel_p_prepare____T__isize(operations__0 []_goml_m_std_p_channel_p_Operation____isize) _goml_m_std_p_channel_p_PreparedOperations____isize {
    var t563 int
    var inline740 int = len(operations__0)
    t563 = inline740
    var kinds__1 *_goml_vec_int
    var inline738 *_goml_vec_int = vec_with_capacity__Vec_3int(t563)
    kinds__1 = inline738
    var receivers__2 *_goml_vec_Receiver_3int
    var inline736 *_goml_vec_Receiver_3int = vec_new__Vec_13Receiver_3int()
    receivers__2 = inline736
    var senders__3 *_goml_vec_Sender_3int
    var inline734 *_goml_vec_Sender_3int = vec_new__Vec_11Sender_3int()
    senders__3 = inline734
    var values__4 *_goml_vec_int
    var inline732 *_goml_vec_int = vec_new__Vec_3int()
    values__4 = inline732
    var for_limit1 int = len(operations__0)
    var for_index2 int = 0
    Loop_loop566:
    for {
        var t567 bool = for_index2 < for_limit1
        if t567 {
            var for_item3 _goml_m_std_p_channel_p_Operation____isize = operations__0[for_index2]
            var t568 int = for_index2 + 1
            for_index2 = t568
            switch for_item3._tag {
            case 0:
                var x5 <-chan int = for_item3._v0_0
                var inline722 int = 0
                vec_push__Vec_3int(kinds__1, inline722)
                vec_push__Vec_13Receiver_3int(receivers__2, x5)
                continue
            case 1:
                var x6 chan<- int = for_item3._v1_0
                var x7 int = for_item3._v1_1
                var inline729 int = 1
                vec_push__Vec_3int(kinds__1, inline729)
                vec_push__Vec_11Sender_3int(senders__3, x6)
                vec_push__Vec_3int(values__4, x7)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop566
        }
    }
    var t565 _goml_m_std_p_channel_p_PreparedOperations____isize = _goml_m_std_p_channel_p_PreparedOperations____isize{
        kinds: kinds__1,
        receivers: receivers__2,
        senders: senders__3,
        values: values__4,
    }
    return t565
}

func _goml_m_std_p_channel_p_prepared__slices____T__isize(prepared__9 _goml_m_std_p_channel_p_PreparedOperations____isize) Tuple4_10Slice_3int_21Slice_13Receiver_3int_19Slice_11Sender_3int_10Slice_3int {
    var t574 *_goml_vec_int = prepared__9.kinds
    var t575 *_goml_vec_int = prepared__9.kinds
    var t576 int
    var inline748 int = vec_len__Vec_3int(t575)
    t576 = inline748
    var t577 []int = t574.items[0:t576]
    var t578 *_goml_vec_Receiver_3int = prepared__9.receivers
    var t579 *_goml_vec_Receiver_3int = prepared__9.receivers
    var t580 int
    var inline746 int = vec_len__Vec_13Receiver_3int(t579)
    t580 = inline746
    var t581 []<-chan int = t578.items[0:t580]
    var t582 *_goml_vec_Sender_3int = prepared__9.senders
    var t583 *_goml_vec_Sender_3int = prepared__9.senders
    var t584 int
    var inline744 int = vec_len__Vec_11Sender_3int(t583)
    t584 = inline744
    var t585 []chan<- int = t582.items[0:t584]
    var t586 *_goml_vec_int = prepared__9.values
    var t587 *_goml_vec_int = prepared__9.values
    var t588 int
    var inline742 int = vec_len__Vec_3int(t587)
    t588 = inline742
    var t589 []int = t586.items[0:t588]
    var t590 Tuple4_10Slice_3int_21Slice_13Receiver_3int_19Slice_11Sender_3int_10Slice_3int = Tuple4_10Slice_3int_21Slice_13Receiver_3int_19Slice_11Sender_3int_10Slice_3int{
        _0: t577,
        _1: t581,
        _2: t585,
        _3: t589,
    }
    return t590
}

func main() {
    main0()
}
