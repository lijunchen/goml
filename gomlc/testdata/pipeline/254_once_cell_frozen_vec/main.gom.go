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
    var inline300 closure_env_values_0 = closure_env_values_0{}
    var inline301 func() FrozenVec__int = func() FrozenVec__int {
        return _goml_m_inherent_i_closure__env__values__0_i_closure__env__values__0_i_apply(inline300)
    }
    var inline302 FrozenVec__int = _goml_m_inherent_i_OnceCell_i__hd25730924ff6191d03d6c0a0b7510106_zenVec_l_int_r_(VALUES, inline301)
    frozen__0 = inline302
    var copy__1 *_goml_vec_int
    var inline297 *_goml_vec_int = frozen__0.values
    var inline298 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__int(inline297)
    copy__1 = inline298
    var inline293 int = 0
    var inline294 int = 9
    vec_set__Vec_3int(copy__1, inline293, inline294)
    var t200 int
    var inline289 int = 0
    var inline290 *_goml_vec_int = frozen__0.values
    var inline291 int = vec_get__Vec_3int(inline290, inline289)
    t200 = inline291
    var t201 string
    var inline287 string = _goml_runtime_core_int_to_string(t200)
    t201 = inline287
    var inline284 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline284)
    var t202 int
    var inline281 int = 0
    var inline282 int = vec_get__Vec_3int(copy__1, inline281)
    t202 = inline282
    var t203 string
    var inline279 string = _goml_runtime_core_int_to_string(t202)
    t203 = inline279
    var inline276 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
    _goml_runtime_core_string_println(inline276)
    var t204 FrozenVec__int
    var inline272 closure_env_values_0 = closure_env_values_0{}
    var inline273 func() FrozenVec__int = func() FrozenVec__int {
        return _goml_m_inherent_i_closure__env__values__0_i_closure__env__values__0_i_apply(inline272)
    }
    var inline274 FrozenVec__int = _goml_m_inherent_i_OnceCell_i__hd25730924ff6191d03d6c0a0b7510106_zenVec_l_int_r_(VALUES, inline273)
    t204 = inline274
    var t205 int
    var inline268 int = 1
    var inline269 *_goml_vec_int = t204.values
    var inline270 int = vec_get__Vec_3int(inline269, inline268)
    t205 = inline270
    var t206 string
    var inline266 string = _goml_runtime_core_int_to_string(t205)
    t206 = inline266
    var inline263 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
    _goml_runtime_core_string_println(inline263)
    return struct{}{}
}

func _goml_m_inherent_i_OnceCell_i__hd25730924ff6191d03d6c0a0b7510106_zenVec_l_int_r_(self__244 *OnceCell__FrozenVec__int, init__245 func() FrozenVec__int) FrozenVec__int {
    var t219 FrozenVec__int = once_cell_get_or_init__OnceCell__FrozenVec__int(self__244, init__245)
    return t219
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__int(self__180 *_goml_vec_int) *_goml_vec_int {
    var t241 int
    var inline311 int = vec_len__Vec_3int(self__180)
    t241 = inline311
    var result__181 *_goml_vec_int
    var inline309 *_goml_vec_int = vec_with_capacity__Vec_3int(t241)
    result__181 = inline309
    var index__182 int = 0
    Loop_loop243:
    for {
        var t244 int
        var inline307 int = vec_len__Vec_3int(self__180)
        t244 = inline307
        var t245 bool = index__182 < t244
        if t245 {
            var t246 int = vec_get__Vec_3int(self__180, index__182)
            vec_push__Vec_3int(result__181, t246)
            var compound_old86 int = index__182
            var compound_value87 int = 1
            var t247 int = compound_old86 + compound_value87
            index__182 = t247
            continue
        } else {
            break Loop_loop243
        }
    }
    return result__181
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__values__0_i_closure__env__values__0_i_apply(env193 closure_env_values_0) FrozenVec__int {
    var vec_literal__118 *_goml_vec_int
    var inline325 *_goml_vec_int = vec_new__Vec_3int()
    vec_literal__118 = inline325
    var inline322 int = 1
    vec_push__Vec_3int(vec_literal__118, inline322)
    var inline319 int = 2
    vec_push__Vec_3int(vec_literal__118, inline319)
    var inline316 int = 3
    vec_push__Vec_3int(vec_literal__118, inline316)
    var inline313 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_copy____T__int(vec_literal__118)
    var inline314 FrozenVec__int = FrozenVec__int{
        values: inline313,
    }
    return inline314
}

func main() {
    main0()
}
