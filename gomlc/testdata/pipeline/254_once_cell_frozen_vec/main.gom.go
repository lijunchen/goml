package main

import (
    _goml_fmt "fmt"
    _goml_slices "slices"
    _goml_sync "sync"
    _goml_runtime_pkg "runtime"
)

func _goml_once_cell_goroutine_id() uint64 {
    var buffer []uint8 = make([]uint8, 64)
    var length int = _goml_runtime_pkg.Stack(buffer, false)
    var index int = 10
    var result uint64 = 0
    for {
        if index >= length {
            break
        }
        if buffer[index] < 48 || buffer[index] > 57 {
            break
        }
        result = result * 10 + uint64(buffer[index] - 48)
        index = index + 1
    }
    return result
}

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func once_cell_new__OnceCell__FrozenVec__int() *OnceCell__FrozenVec__int {
    var cell *OnceCell__FrozenVec__int = &OnceCell__FrozenVec__int{}
    cell.cond = _goml_sync.NewCond(&cell.mutex)
    return cell
}

func once_cell_get_or_init__OnceCell__FrozenVec__int(cell *OnceCell__FrozenVec__int, init func() FrozenVec__int) FrozenVec__int {
    var goroutine uint64 = _goml_once_cell_goroutine_id()
    cell.mutex.Lock()
    for {
        if cell.state == 2 {
            cell.mutex.Unlock()
            return cell.value
        }
        if cell.state == 1 {
            if cell.owner == goroutine {
                cell.mutex.Unlock()
                panic("recursive OnceCell initialization: " + cell.name)
            }
            cell.cond.Wait()
            continue
        }
        cell.state = 1
        cell.owner = goroutine
        cell.mutex.Unlock()
        var initialized FrozenVec__int = init()
        cell.mutex.Lock()
        cell.value = initialized
        cell.state = 2
        cell.owner = 0
        cell.cond.Broadcast()
        cell.mutex.Unlock()
        return initialized
    }
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
        items: _goml_slices.Grow([]int{}, int(capacity)),
    }
}

func vec_push__Vec_3int(vec *_goml_vec_int, elem int) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_3int(vec *_goml_vec_int, index int) int {
    return vec.items[index]
}

func vec_set__Vec_3int(vec *_goml_vec_int, index int, value int) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_3int(vec *_goml_vec_int) int {
    return int(len(vec.items))
}

type closure_env_values_0 struct {}

type FrozenVec__int struct {
    values *_goml_vec_int
}

type OnceCell__FrozenVec__int struct {
    mutex _goml_sync.Mutex
    cond *_goml_sync.Cond
    state int
    owner uint64
    value FrozenVec__int
    name string
}

type Ordering int32

var VALUES *OnceCell__FrozenVec__int = func() *OnceCell__FrozenVec__int {
    var cell *OnceCell__FrozenVec__int = once_cell_new__OnceCell__FrozenVec__int()
    cell.name = "VALUES"
    return cell
}()

func main0() struct{} {
    var frozen__0 FrozenVec__int
    var inline521 closure_env_values_0 = closure_env_values_0{}
    var inline522 func() FrozenVec__int = func() FrozenVec__int {
        return _goml_m_inherent_i_closure__env__values__0_i_closure__env__values__0_i_apply(inline521)
    }
    var inline523 FrozenVec__int = _goml_m_inherent_i_OnceCell_i__hd25730924ff6191d03d6c0a0b7510106_zenVec_l_int_r_(VALUES, inline522)
    frozen__0 = inline523
    var copy__1 *_goml_vec_int
    var inline518 *_goml_vec_int = frozen__0.values
    var inline519 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__int(inline518)
    copy__1 = inline519
    var inline514 int = 0
    var inline515 int = 9
    vec_set__Vec_3int(copy__1, inline514, inline515)
    var t421 int
    var inline510 int = 0
    var inline511 *_goml_vec_int = frozen__0.values
    var inline512 int = vec_get__Vec_3int(inline511, inline510)
    t421 = inline512
    var t422 string
    var inline508 string = _goml_runtime_core_int_to_string(t421)
    t422 = inline508
    var inline505 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t422)
    _goml_runtime_core_string_println(inline505)
    var t423 int
    var inline502 int = 0
    var inline503 int = vec_get__Vec_3int(copy__1, inline502)
    t423 = inline503
    var t424 string
    var inline500 string = _goml_runtime_core_int_to_string(t423)
    t424 = inline500
    var inline497 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t424)
    _goml_runtime_core_string_println(inline497)
    var t425 FrozenVec__int
    var inline493 closure_env_values_0 = closure_env_values_0{}
    var inline494 func() FrozenVec__int = func() FrozenVec__int {
        return _goml_m_inherent_i_closure__env__values__0_i_closure__env__values__0_i_apply(inline493)
    }
    var inline495 FrozenVec__int = _goml_m_inherent_i_OnceCell_i__hd25730924ff6191d03d6c0a0b7510106_zenVec_l_int_r_(VALUES, inline494)
    t425 = inline495
    var t426 int
    var inline489 int = 1
    var inline490 *_goml_vec_int = t425.values
    var inline491 int = vec_get__Vec_3int(inline490, inline489)
    t426 = inline491
    var t427 string
    var inline487 string = _goml_runtime_core_int_to_string(t426)
    t427 = inline487
    var inline484 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t427)
    _goml_runtime_core_string_println(inline484)
    return struct{}{}
}

func _goml_m_inherent_i_OnceCell_i__hd25730924ff6191d03d6c0a0b7510106_zenVec_l_int_r_(self__402 *OnceCell__FrozenVec__int, init__403 func() FrozenVec__int) FrozenVec__int {
    var t440 FrozenVec__int = once_cell_get_or_init__OnceCell__FrozenVec__int(self__402, init__403)
    return t440
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__int(self__264 *_goml_vec_int) *_goml_vec_int {
    var t462 int
    var inline532 int = vec_len__Vec_3int(self__264)
    t462 = inline532
    var result__265 *_goml_vec_int
    var inline530 *_goml_vec_int = vec_with_capacity__Vec_3int(t462)
    result__265 = inline530
    var index__266 int = 0
    Loop_loop464:
    for {
        var t465 int
        var inline528 int = vec_len__Vec_3int(self__264)
        t465 = inline528
        var t466 bool = index__266 < t465
        if t466 {
            var t467 int = vec_get__Vec_3int(self__264, index__266)
            vec_push__Vec_3int(result__265, t467)
            var compound_old196 int = index__266
            var compound_value197 int = 1
            var t468 int = compound_old196 + compound_value197
            index__266 = t468
            continue
        } else {
            break Loop_loop464
        }
    }
    return result__265
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__values__0_i_closure__env__values__0_i_apply(env414 closure_env_values_0) FrozenVec__int {
    var vec_literal__118 *_goml_vec_int
    var inline546 *_goml_vec_int = vec_new__Vec_3int()
    vec_literal__118 = inline546
    var inline543 int = 1
    vec_push__Vec_3int(vec_literal__118, inline543)
    var inline540 int = 2
    vec_push__Vec_3int(vec_literal__118, inline540)
    var inline537 int = 3
    vec_push__Vec_3int(vec_literal__118, inline537)
    var inline534 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__int(vec_literal__118)
    var inline535 FrozenVec__int = FrozenVec__int{
        values: inline534,
    }
    return inline535
}

func main() {
    main0()
}
