package main

func main() {
	sum := 0
	for index, value := range []int{2, 3, 4} {
		sum += index + value
	}
	count := 0
	for range 3 {
		count++
	}
	integerSum := 0
	for index := range 4 {
		integerSum += index
	}
	array := [2]int{5, 6}
	arraySum := 0
	for index, value := range array {
		array[1] = 99
		arraySum += index + value
	}
	pointerSum := 0
	pointer := &array
	for index, value := range pointer {
		pointer[index]++
		pointerSum += value
	}
	slice := []int{1, 2}
	sliceCount := 0
	for index, value := range slice {
		if index == 0 {
			slice = append(slice, 3)
		}
		sliceCount += value
	}
	println(sum, count, integerSum, arraySum, pointerSum, sliceCount)
	for index, value := range "\xffA界" {
		println(index, value)
	}
}
