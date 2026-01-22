package Main
file main.gom
  struct Counter {
        start: uint32,
        end: uint64,
    }
    
    fn is_flag8(local/1/0: uint8) -> bool {
      match value/0 {
          0u8 => true,
          200u8 => true,
          _ => false,
      };
    }
    
    fn is_flag16(local/1/1: uint16) -> bool {
      match value/1 {
          1024u16 => true,
          65000u16 => true,
          _ => false,
      };
    }
    
    fn is_flag32(local/1/2: uint32) -> bool {
      match value/2 {
          4000000000u32 => true,
          1234567890u32 => true,
          _ => false,
      };
    }
    
    fn is_flag64(local/1/3: uint64) -> bool {
      match value/3 {
          900000000u64 => true,
          600000000u64 => true,
          _ => false,
      };
    }
    
    fn match_struct(local/1/4: Counter) -> bool {
      match counter/4 {
          Counter { start: 4000000000u32, end: 900000000u64 } => true,
          Counter { start: _, end: 600000000u64 } => true,
          _ => false,
      };
    }
    
    fn report(local/1/5: string, local/1/6: bool) -> string {
      label/5 + bool_to_string(value/6);
    }
    
    fn main() {
      let local/1/7 = Counter {
          start: 4000000000u32,
          end: 900000000u64
      };
      let local/1/8 = Counter {
          start: 12u32,
          end: 600000000u64
      };
      let local/1/9 = def/1/6("u8_hit=", def/1/1(200u8)) + def/1/6(",u8_miss=", def/1/1(15u8)) + def/1/6(",u16_hit=", def/1/2(65000u16)) + def/1/6(",u16_miss=", def/1/2(42u16)) + def/1/6(",u32_hit=", def/1/3(1234567890u32)) + def/1/6(",u32_miss=", def/1/3(99u32)) + def/1/6(",u64_hit=", def/1/4(900000000u64)) + def/1/6(",u64_miss=", def/1/4(700000000u64)) + def/1/6(",struct_first=", def/1/5(counter/7)) + def/1/6(",struct_second=", def/1/5(alt_counter/8));
      let _ = string_println(message/9);
      ();
    }