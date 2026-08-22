package main

import (
    _goml_os "os"
    _goml_reflect "reflect"
)

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
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

type _goml_vec_uint8 struct {
    items []uint8
}

func vec_with_capacity__Vec_5uint8(capacity int) *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: make([]uint8, 0, capacity),
    }
}

func vec_push__Vec_5uint8(vec *_goml_vec_uint8, elem uint8) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5uint8(vec *_goml_vec_uint8, index int) uint8 {
    return vec.items[index]
}

func vec_len__Vec_5uint8(vec *_goml_vec_uint8) int {
    return int(len(vec.items))
}

type _goml_vec_uint32 struct {
    items []uint32
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

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type FloatNatural struct {
    words *_goml_vec_uint32
}

type ParsedFloat struct {
    valid bool
    negative bool
    special int
    numerator FloatNatural
    decimal_exponent int
    binary_exponent int
    hexadecimal bool
    significant_digits int
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
        var x796 int = value__0.(Received)._0
        var x797 Option__isize = value__0.(Received)._1
        var inline1064 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(x796)
        _goml_runtime_core_string_println(inline1064)
        var t827 int
        var inline1060 int = -1
        switch x797._tag {
        case 0:
            t827 = inline1060
        case 1:
            var inline1061 int = x797._v1_0
            t827 = inline1061
        default:
            panic("non-exhaustive match")
        }
        var inline1057 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t827)
        _goml_runtime_core_string_println(inline1057)
        return struct{}{}
    case Sent:
        var x798 int = value__0.(Sent)._0
        var inline1067 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(x798)
        _goml_runtime_core_string_println(inline1067)
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
    var t831 <-chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_receiver____T__isize(first__4)
    var t832 _goml_m_std_p_channel_p_Operation____isize = _goml_m_std_p_channel_p_Operation____isize{
        _tag: 0,
        _v0_0: t831,
    }
    var t833 <-chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_receiver____T__isize(second__5)
    var t834 _goml_m_std_p_channel_p_Operation____isize = _goml_m_std_p_channel_p_Operation____isize{
        _tag: 0,
        _v0_0: t833,
    }
    var t835 [2]_goml_m_std_p_channel_p_Operation____isize = [2]_goml_m_std_p_channel_p_Operation____isize{t832, t834}
    var operations__6 *_goml_vec__goml_m_std_p_channel_p_Operation____isize = func(values [2]_goml_m_std_p_channel_p_Operation____isize) *_goml_vec__goml_m_std_p_channel_p_Operation____isize {
        var storage struct {
            vector _goml_vec__goml_m_std_p_channel_p_Operation____isize
            values [2]_goml_m_std_p_channel_p_Operation____isize
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t835)
    var t836 int = _goml_m_inherent_i_Vec_i_Vec_l_h6161506801e1e3b7949d66d7b76c6b61_tion_l_isize_r_(operations__6)
    var t837 []_goml_m_std_p_channel_p_Operation____isize = operations__6.items[0:t836]
    var mtmp802 _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError = _goml_m_std_p_channel_p_try__select__priority____T__isize(t837)
    switch mtmp802._tag {
    case 0:
        var x803 _goml_m_Option____std_p_channel_p_Selection____isize = mtmp802._v0_0
        switch x803._tag {
        case 1:
            var x805 _goml_m_std_p_channel_p_Selection____isize = x803._v1_0
            switch x805.(type) {
            case Received:
                var inline1070 int = x805.(Received)._0
                var inline1071 Option__isize = x805.(Received)._1
                println__T_isize(inline1070)
                var inline1075 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__isize(inline1071, -1)
                println__T_isize(inline1075)
            case Sent:
                var inline1077 int = x805.(Sent)._0
                println__T_isize(inline1077)
            default:
                panic("non-exhaustive match")
            }
        default:
            var inline1081 int = -1
            var inline1082 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline1081)
            _goml_runtime_core_string_println(inline1082)
        }
    default:
        var inline1085 int = -1
        var inline1086 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline1085)
        _goml_runtime_core_string_println(inline1086)
    }
    var t839 int = _goml_m_inherent_i_Vec_i_Vec_l_h6161506801e1e3b7949d66d7b76c6b61_tion_l_isize_r_(operations__6)
    var t840 []_goml_m_std_p_channel_p_Operation____isize = operations__6.items[0:t839]
    var mtmp807 _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError = _goml_m_std_p_channel_p_try__select____T__isize(t840)
    switch mtmp807._tag {
    case 0:
        var x808 _goml_m_Option____std_p_channel_p_Selection____isize = mtmp807._v0_0
        switch x808._tag {
        case 1:
            var x810 _goml_m_std_p_channel_p_Selection____isize = x808._v1_0
            switch x810.(type) {
            case Received:
                var inline1089 int = x810.(Received)._0
                var inline1090 Option__isize = x810.(Received)._1
                println__T_isize(inline1089)
                var inline1094 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__isize(inline1090, -1)
                println__T_isize(inline1094)
            case Sent:
                var inline1096 int = x810.(Sent)._0
                println__T_isize(inline1096)
            default:
                panic("non-exhaustive match")
            }
        default:
            var inline1100 int = -2
            var inline1101 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline1100)
            _goml_runtime_core_string_println(inline1101)
        }
    default:
        var inline1104 int = -2
        var inline1105 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline1104)
        _goml_runtime_core_string_println(inline1105)
    }
    var empty__9 chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__isize(0)
    var target__10 chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__isize(1)
    var t842 <-chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_receiver____T__isize(empty__9)
    var t843 _goml_m_std_p_channel_p_Operation____isize = _goml_m_std_p_channel_p_Operation____isize{
        _tag: 0,
        _v0_0: t842,
    }
    var t844 chan<- int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_sender____T__isize(target__10)
    var t845 _goml_m_std_p_channel_p_Operation____isize = _goml_m_std_p_channel_p_Operation____isize{
        _tag: 1,
        _v1_0: t844,
        _v1_1: 30,
    }
    var t846 [2]_goml_m_std_p_channel_p_Operation____isize = [2]_goml_m_std_p_channel_p_Operation____isize{t843, t845}
    var mixed__11 *_goml_vec__goml_m_std_p_channel_p_Operation____isize = func(values [2]_goml_m_std_p_channel_p_Operation____isize) *_goml_vec__goml_m_std_p_channel_p_Operation____isize {
        var storage struct {
            vector _goml_vec__goml_m_std_p_channel_p_Operation____isize
            values [2]_goml_m_std_p_channel_p_Operation____isize
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t846)
    var t847 int = _goml_m_inherent_i_Vec_i_Vec_l_h6161506801e1e3b7949d66d7b76c6b61_tion_l_isize_r_(mixed__11)
    var t848 []_goml_m_std_p_channel_p_Operation____isize = mixed__11.items[0:t847]
    var mtmp812 _goml_m_Result____std_p_channe_h5400d2fb6c865f5b36aaa9b0f9e787f7_l_p_SelectError = _goml_m_std_p_channel_p_select____T__isize(t848)
    switch mtmp812._tag {
    case 0:
        var x813 _goml_m_std_p_channel_p_Selection____isize = mtmp812._v0_0
        print_selection(x813)
    case 1:
        var inline1108 int = -3
        var inline1109 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline1108)
        _goml_runtime_core_string_println(inline1109)
    default:
        panic("non-exhaustive match")
    }
    var t850 Option__isize = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__isize(target__10)
    var t851 int = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__isize(t850, -4)
    println__T_isize(t851)
    var t852 <-chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_receiver____T__isize(empty__9)
    var t853 _goml_m_std_p_channel_p_Operation____isize = _goml_m_std_p_channel_p_Operation____isize{
        _tag: 0,
        _v0_0: t852,
    }
    var t854 [1]_goml_m_std_p_channel_p_Operation____isize = [1]_goml_m_std_p_channel_p_Operation____isize{t853}
    var none__13 *_goml_vec__goml_m_std_p_channel_p_Operation____isize = func(values [1]_goml_m_std_p_channel_p_Operation____isize) *_goml_vec__goml_m_std_p_channel_p_Operation____isize {
        var storage struct {
            vector _goml_vec__goml_m_std_p_channel_p_Operation____isize
            values [1]_goml_m_std_p_channel_p_Operation____isize
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t854)
    var t855 int = _goml_m_inherent_i_Vec_i_Vec_l_h6161506801e1e3b7949d66d7b76c6b61_tion_l_isize_r_(none__13)
    var t856 []_goml_m_std_p_channel_p_Operation____isize = none__13.items[0:t855]
    var mtmp817 _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError = _goml_m_std_p_channel_p_try__select____T__isize(t856)
    var jp858 int
    switch mtmp817._tag {
    case 0:
        var x818 _goml_m_Option____std_p_channel_p_Selection____isize = mtmp817._v0_0
        switch x818._tag {
        case 0:
            jp858 = 40
        default:
            jp858 = -5
        }
    default:
        jp858 = -5
    }
    println__T_isize(jp858)
    var no_operations__14 *_goml_vec__goml_m_std_p_channel_p_Operation____isize
    var inline1117 *_goml_vec__goml_m_std_p_channel_p_Operation____isize = vec_new___goml_m_Vec__30std_p_channel_p_Operation____isize()
    no_operations__14 = inline1117
    var t859 int
    var inline1115 int = vec_len___goml_m_Vec__30std_p_channel_p_Operation____isize(no_operations__14)
    t859 = inline1115
    var t860 []_goml_m_std_p_channel_p_Operation____isize = no_operations__14.items[0:t859]
    var mtmp822 _goml_m_Result____std_p_channe_h5400d2fb6c865f5b36aaa9b0f9e787f7_l_p_SelectError = _goml_m_std_p_channel_p_select____T__isize(t860)
    var jp862 int
    switch mtmp822._tag {
    case 1:
        var x824 _goml_m_std_p_channel_p_SelectError = mtmp822._v1_0
        switch x824 {
        case Empty:
            jp862 = 50
        default:
            panic("non-exhaustive match")
        }
    default:
        jp862 = -6
    }
    var inline1112 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(jp862)
    _goml_runtime_core_string_println(inline1112)
    return struct{}{}
}

func println__T_isize(value__1 int) struct{} {
    var t879 string
    var inline1119 string = __goml_builtin_int_to_string(value__1)
    t879 = inline1119
    _goml_runtime_core_string_println(t879)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__isize(self__720 Option__isize, fallback__721 int) int {
    switch self__720._tag {
    case 0:
        return fallback__721
    case 1:
        var x775 int = self__720._v1_0
        return x775
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__isize(capacity__688 int) chan int {
    var t887 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(capacity__688)
    return t887
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__isize(self__689 chan int, value__690 int) struct{} {
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(self__689, value__690)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_receiver____T__isize(self__696 chan int) <-chan int {
    var t892 <-chan int = func(p0 chan int) <-chan int {
        return p0
    }(self__696)
    return t892
}

func _goml_m_std_p_channel_p_try__select__priority____T__isize(operations__35 []_goml_m_std_p_channel_p_Operation____isize) _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError {
    var t897 int
    var inline1128 int = len(operations__35)
    t897 = inline1128
    var t898 bool = t897 == 0
    if t898 {
        var t899 _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError = _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError{
            _tag: 1,
            _v1_0: Empty,
        }
        return t899
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
        var jp901 _goml_m_Option____std_p_channel_p_Selection____isize
        if x44 {
            var t903 _goml_m_std_p_channel_p_Selection____isize
            var inline1121 bool = x41 == 0
            if inline1121 {
                var inline1123 Option__isize
                if x43 {
                    var inline1125 Option__isize = Option__isize{
                        _tag: 1,
                        _v1_0: x42,
                    }
                    inline1123 = inline1125
                } else {
                    inline1123 = Option__isize{
                        _tag: 0,
                    }
                }
                var inline1124 _goml_m_std_p_channel_p_Selection____isize = Received{
                    _0: x40,
                    _1: inline1123,
                }
                t903 = inline1124
            } else {
                var inline1126 _goml_m_std_p_channel_p_Selection____isize = Sent{
                    _0: x40,
                }
                t903 = inline1126
            }
            var t904 _goml_m_Option____std_p_channel_p_Selection____isize = _goml_m_Option____std_p_channel_p_Selection____isize{
                _tag: 1,
                _v1_0: t903,
            }
            jp901 = t904
        } else {
            jp901 = _goml_m_Option____std_p_channel_p_Selection____isize{
                _tag: 0,
            }
        }
        var t902 _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError = _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError{
            _tag: 0,
            _v0_0: jp901,
        }
        return t902
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_h6161506801e1e3b7949d66d7b76c6b61_tion_l_isize_r_(self__526 *_goml_vec__goml_m_std_p_channel_p_Operation____isize) int {
    var t907 int = vec_len___goml_m_Vec__30std_p_channel_p_Operation____isize(self__526)
    return t907
}

func _goml_m_std_p_channel_p_try__select____T__isize(operations__24 []_goml_m_std_p_channel_p_Operation____isize) _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError {
    var t912 int
    var inline1137 int = len(operations__24)
    t912 = inline1137
    var t913 bool = t912 == 0
    if t913 {
        var t914 _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError = _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError{
            _tag: 1,
            _v1_0: Empty,
        }
        return t914
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
        var jp916 _goml_m_Option____std_p_channel_p_Selection____isize
        if x33 {
            var t918 _goml_m_std_p_channel_p_Selection____isize
            var inline1130 bool = x30 == 0
            if inline1130 {
                var inline1132 Option__isize
                if x32 {
                    var inline1134 Option__isize = Option__isize{
                        _tag: 1,
                        _v1_0: x31,
                    }
                    inline1132 = inline1134
                } else {
                    inline1132 = Option__isize{
                        _tag: 0,
                    }
                }
                var inline1133 _goml_m_std_p_channel_p_Selection____isize = Received{
                    _0: x29,
                    _1: inline1132,
                }
                t918 = inline1133
            } else {
                var inline1135 _goml_m_std_p_channel_p_Selection____isize = Sent{
                    _0: x29,
                }
                t918 = inline1135
            }
            var t919 _goml_m_Option____std_p_channel_p_Selection____isize = _goml_m_Option____std_p_channel_p_Selection____isize{
                _tag: 1,
                _v1_0: t918,
            }
            jp916 = t919
        } else {
            jp916 = _goml_m_Option____std_p_channel_p_Selection____isize{
                _tag: 0,
            }
        }
        var t917 _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError = _goml_m_Result____Option____st_ha315a4af527ac57871bea61a2ab15462_l_p_SelectError{
            _tag: 0,
            _v0_0: jp916,
        }
        return t917
    }
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_sender____T__isize(self__695 chan int) chan<- int {
    var t922 chan<- int = func(p0 chan int) chan<- int {
        return p0
    }(self__695)
    return t922
}

func _goml_m_std_p_channel_p_select____T__isize(operations__14 []_goml_m_std_p_channel_p_Operation____isize) _goml_m_Result____std_p_channe_h5400d2fb6c865f5b36aaa9b0f9e787f7_l_p_SelectError {
    var t927 int
    var inline1146 int = len(operations__14)
    t927 = inline1146
    var t928 bool = t927 == 0
    if t928 {
        var t929 _goml_m_Result____std_p_channe_h5400d2fb6c865f5b36aaa9b0f9e787f7_l_p_SelectError = _goml_m_Result____std_p_channe_h5400d2fb6c865f5b36aaa9b0f9e787f7_l_p_SelectError{
            _tag: 1,
            _v1_0: Empty,
        }
        return t929
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
        var t930 _goml_m_std_p_channel_p_Selection____isize
        var inline1139 bool = x19 == 0
        if inline1139 {
            var inline1141 Option__isize
            if x21 {
                var inline1143 Option__isize = Option__isize{
                    _tag: 1,
                    _v1_0: x20,
                }
                inline1141 = inline1143
            } else {
                inline1141 = Option__isize{
                    _tag: 0,
                }
            }
            var inline1142 _goml_m_std_p_channel_p_Selection____isize = Received{
                _0: x18,
                _1: inline1141,
            }
            t930 = inline1142
        } else {
            var inline1144 _goml_m_std_p_channel_p_Selection____isize = Sent{
                _0: x18,
            }
            t930 = inline1144
        }
        var t931 _goml_m_Result____std_p_channe_h5400d2fb6c865f5b36aaa9b0f9e787f7_l_p_SelectError = _goml_m_Result____std_p_channe_h5400d2fb6c865f5b36aaa9b0f9e787f7_l_p_SelectError{
            _tag: 0,
            _v0_0: t930,
        }
        return t931
    }
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__isize(self__691 chan int) Option__isize {
    var mtmp764 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(self__691)
    var x765 int = mtmp764._0
    var x766 bool = mtmp764._1
    if x766 {
        var t936 Option__isize = Option__isize{
            _tag: 1,
            _v1_0: x765,
        }
        return t936
    } else {
        return Option__isize{
            _tag: 0,
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline1148 int64 = int64(int(self__404))
    var inline1149 string = signed_decimal_string(inline1148)
    return inline1149
}

func _goml_m_std_p_channel_p_prepare____T__isize(operations__0 []_goml_m_std_p_channel_p_Operation____isize) _goml_m_std_p_channel_p_PreparedOperations____isize {
    var t948 int
    var inline1171 int = len(operations__0)
    t948 = inline1171
    var kinds__1 *_goml_vec_int
    var inline1169 *_goml_vec_int = vec_with_capacity__Vec_3int(t948)
    kinds__1 = inline1169
    var receivers__2 *_goml_vec_Receiver_3int
    var inline1167 *_goml_vec_Receiver_3int = vec_new__Vec_13Receiver_3int()
    receivers__2 = inline1167
    var senders__3 *_goml_vec_Sender_3int
    var inline1165 *_goml_vec_Sender_3int = vec_new__Vec_11Sender_3int()
    senders__3 = inline1165
    var values__4 *_goml_vec_int
    var inline1163 *_goml_vec_int = vec_new__Vec_3int()
    values__4 = inline1163
    var for_limit1 int = len(operations__0)
    var for_index2 int = 0
    Loop_loop951:
    for {
        var t952 bool = for_index2 < for_limit1
        if t952 {
            var for_item3 _goml_m_std_p_channel_p_Operation____isize = operations__0[for_index2]
            var t953 int = for_index2 + 1
            for_index2 = t953
            switch for_item3._tag {
            case 0:
                var x5 <-chan int = for_item3._v0_0
                var inline1153 int = 0
                vec_push__Vec_3int(kinds__1, inline1153)
                vec_push__Vec_13Receiver_3int(receivers__2, x5)
                continue
            case 1:
                var x6 chan<- int = for_item3._v1_0
                var x7 int = for_item3._v1_1
                var inline1160 int = 1
                vec_push__Vec_3int(kinds__1, inline1160)
                vec_push__Vec_11Sender_3int(senders__3, x6)
                vec_push__Vec_3int(values__4, x7)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop951
        }
    }
    var t950 _goml_m_std_p_channel_p_PreparedOperations____isize = _goml_m_std_p_channel_p_PreparedOperations____isize{
        kinds: kinds__1,
        receivers: receivers__2,
        senders: senders__3,
        values: values__4,
    }
    return t950
}

func _goml_m_std_p_channel_p_prepared__slices____T__isize(prepared__9 _goml_m_std_p_channel_p_PreparedOperations____isize) Tuple4_10Slice_3int_21Slice_13Receiver_3int_19Slice_11Sender_3int_10Slice_3int {
    var t959 *_goml_vec_int = prepared__9.kinds
    var t960 *_goml_vec_int = prepared__9.kinds
    var t961 int
    var inline1179 int = vec_len__Vec_3int(t960)
    t961 = inline1179
    var t962 []int = t959.items[0:t961]
    var t963 *_goml_vec_Receiver_3int = prepared__9.receivers
    var t964 *_goml_vec_Receiver_3int = prepared__9.receivers
    var t965 int
    var inline1177 int = vec_len__Vec_13Receiver_3int(t964)
    t965 = inline1177
    var t966 []<-chan int = t963.items[0:t965]
    var t967 *_goml_vec_Sender_3int = prepared__9.senders
    var t968 *_goml_vec_Sender_3int = prepared__9.senders
    var t969 int
    var inline1175 int = vec_len__Vec_11Sender_3int(t968)
    t969 = inline1175
    var t970 []chan<- int = t967.items[0:t969]
    var t971 *_goml_vec_int = prepared__9.values
    var t972 *_goml_vec_int = prepared__9.values
    var t973 int
    var inline1173 int = vec_len__Vec_3int(t972)
    t973 = inline1173
    var t974 []int = t971.items[0:t973]
    var t975 Tuple4_10Slice_3int_21Slice_13Receiver_3int_19Slice_11Sender_3int_10Slice_3int = Tuple4_10Slice_3int_21Slice_13Receiver_3int_19Slice_11Sender_3int_10Slice_3int{
        _0: t962,
        _1: t966,
        _2: t970,
        _3: t974,
    }
    return t975
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t988 int64 = int64(int(value__222))
    var inline1181 bool = t988 < 0
    if inline1181 {
        var inline1182 uint64 = uint64(int64(t988))
        var inline1183 uint64 = 0 - inline1182
        var inline1184 string = decimal_string(inline1183)
        var inline1185 string = "-" + inline1184
        return inline1185
    } else {
        var inline1186 uint64 = uint64(int64(t988))
        var inline1187 string = decimal_string(inline1186)
        return inline1187
    }
}

func signed_decimal_string(value__214 int64) string {
    var t1021 bool = value__214 < 0
    if t1021 {
        var t1022 uint64 = uint64(int64(value__214))
        var t1023 uint64 = 0 - t1022
        var t1024 string = decimal_string(t1023)
        var t1025 string = "-" + t1024
        return t1025
    } else {
        var t1026 uint64 = uint64(int64(value__214))
        var t1027 string = decimal_string(t1026)
        return t1027
    }
}

func decimal_string(value__208 uint64) string {
    var t1050 bool = value__208 == 0
    if t1050 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop1043:
        for {
            var t1044 bool = remaining__210 > 0
            if t1044 {
                var t1045_rhs uint64 = 10
                var t1045 uint64 = remaining__210 % t1045_rhs
                var t1046 uint8 = uint8(uint64(t1045))
                var t1047 uint8 = t1046 + 48
                vec_push__Vec_5uint8(reversed__209, t1047)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t1048 uint64 = compound_old353 / compound_value354
                remaining__210 = t1048
                continue
            } else {
                break Loop_loop1043
            }
        }
        var t1032 int
        var inline1197 int = vec_len__Vec_5uint8(reversed__209)
        t1032 = inline1197
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1032)
        var offset__212 int = 0
        Loop_loop1034:
        for {
            var t1035 int
            var inline1195 int = vec_len__Vec_5uint8(reversed__209)
            t1035 = inline1195
            var t1036 bool = offset__212 < t1035
            if t1036 {
                var t1037 int
                var inline1193 int = vec_len__Vec_5uint8(reversed__209)
                t1037 = inline1193
                var t1038 int = t1037 - offset__212
                var t1039 int = t1038 - 1
                var t1040 uint8 = vec_get__Vec_5uint8(reversed__209, t1039)
                vec_push__Vec_5uint8(bytes__211, t1040)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t1041 int = compound_old358 + compound_value359
                offset__212 = t1041
                continue
            } else {
                break Loop_loop1034
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func main() {
    main0()
}
