package goshim

func Decorate(value string) (string, int32) {
	return "<" + value + ">", int32(len(value))
}

func ByteLength(value []byte) int {
	return len(value)
}

func Swap(value [2]int32) [2]int32 {
	return [2]int32{value[1], value[0]}
}

func Send(channel chan string, value string) {
	channel <- value
}
