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

var VALUES *OnceCell__FrozenVec__int = func() *OnceCell__FrozenVec__int {
    var cell *OnceCell__FrozenVec__int = once_cell_new__OnceCell__FrozenVec__int()
    cell.name = "VALUES"
    return cell
}()

func main0() struct{} {
    var frozen__0 FrozenVec__int
    var inline295 closure_env_values_0 = closure_env_values_0{}
    var inline296 func() FrozenVec__int = func() FrozenVec__int {
        return _goml_m_inherent_i_closure__env__values__0_i_closure__env__values__0_i_apply(inline295)
    }
    var inline297 FrozenVec__int = _goml_m_inherent_i_OnceCell_i__hd25730924ff6191d03d6c0a0b7510106_zenVec_l_int_r_(VALUES, inline296)
    frozen__0 = inline297
    var copy__1 *_goml_vec_int
    var inline292 *_goml_vec_int = frozen__0.values
    var inline293 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__int(inline292)
    copy__1 = inline293
    var inline288 int = 0
    var inline289 int = 9
    vec_set__Vec_3int(copy__1, inline288, inline289)
    var t195 int
    var inline284 int = 0
    var inline285 *_goml_vec_int = frozen__0.values
    var inline286 int = vec_get__Vec_3int(inline285, inline284)
    t195 = inline286
    var t196 string
    var inline282 string = _goml_runtime_core_int_to_string(t195)
    t196 = inline282
    var inline279 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline279)
    var t197 int
    var inline276 int = 0
    var inline277 int = vec_get__Vec_3int(copy__1, inline276)
    t197 = inline277
    var t198 string
    var inline274 string = _goml_runtime_core_int_to_string(t197)
    t198 = inline274
    var inline271 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline271)
    var t199 FrozenVec__int
    var inline267 closure_env_values_0 = closure_env_values_0{}
    var inline268 func() FrozenVec__int = func() FrozenVec__int {
        return _goml_m_inherent_i_closure__env__values__0_i_closure__env__values__0_i_apply(inline267)
    }
    var inline269 FrozenVec__int = _goml_m_inherent_i_OnceCell_i__hd25730924ff6191d03d6c0a0b7510106_zenVec_l_int_r_(VALUES, inline268)
    t199 = inline269
    var t200 int
    var inline263 int = 1
    var inline264 *_goml_vec_int = t199.values
    var inline265 int = vec_get__Vec_3int(inline264, inline263)
    t200 = inline265
    var t201 string
    var inline261 string = _goml_runtime_core_int_to_string(t200)
    t201 = inline261
    var inline258 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline258)
    return struct{}{}
}

func _goml_m_inherent_i_OnceCell_i__hd25730924ff6191d03d6c0a0b7510106_zenVec_l_int_r_(self__241 *OnceCell__FrozenVec__int, init__242 func() FrozenVec__int) FrozenVec__int {
    var t214 FrozenVec__int = once_cell_get_or_init__OnceCell__FrozenVec__int(self__241, init__242)
    return t214
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__int(self__180 *_goml_vec_int) *_goml_vec_int {
    var t236 int
    var inline306 int = vec_len__Vec_3int(self__180)
    t236 = inline306
    var result__181 *_goml_vec_int
    var inline304 *_goml_vec_int = vec_with_capacity__Vec_3int(t236)
    result__181 = inline304
    var index__182 int = 0
    Loop_loop238:
    for {
        var t239 int
        var inline302 int = vec_len__Vec_3int(self__180)
        t239 = inline302
        var t240 bool = index__182 < t239
        if t240 {
            var t241 int = vec_get__Vec_3int(self__180, index__182)
            vec_push__Vec_3int(result__181, t241)
            var compound_old86 int = index__182
            var compound_value87 int = 1
            var t242 int = compound_old86 + compound_value87
            index__182 = t242
            continue
        } else {
            break Loop_loop238
        }
    }
    return result__181
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__values__0_i_closure__env__values__0_i_apply(env188 closure_env_values_0) FrozenVec__int {
    var vec_literal__118 *_goml_vec_int
    var inline320 *_goml_vec_int = vec_new__Vec_3int()
    vec_literal__118 = inline320
    var inline317 int = 1
    vec_push__Vec_3int(vec_literal__118, inline317)
    var inline314 int = 2
    vec_push__Vec_3int(vec_literal__118, inline314)
    var inline311 int = 3
    vec_push__Vec_3int(vec_literal__118, inline311)
    var inline308 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__int(vec_literal__118)
    var inline309 FrozenVec__int = FrozenVec__int{
        values: inline308,
    }
    return inline309
}

func main() {
    main0()
}
