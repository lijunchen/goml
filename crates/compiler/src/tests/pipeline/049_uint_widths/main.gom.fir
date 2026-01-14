package Main
file main.gom
  fn main() {
      let local/1/0: uint8 = 200u8;
      let local/1/1: uint8 = 55u8;
      let local/1/2 = start8/0 + add8/1;
      let local/1/3 = -start8/0;
      let local/1/4: uint16 = 50000u16;
      let local/1/5: uint16 = 12000u16;
      let local/1/6 = start16/4 + add16/5;
      let local/1/7 = sum16/6 - start16/4;
      let local/1/8: uint32 = 3000000000u32;
      let local/1/9: uint32 = 123456789u32;
      let local/1/10 = start32/8 + add32/9;
      let local/1/11 = -add32/9;
      let local/1/12: uint64 = 6000000000u64;
      let local/1/13: uint64 = 4000000000u64;
      let local/1/14 = start64/12 + add64/13;
      let local/1/15 = sum64/14 - add64/13;
      let local/1/16 = uint8_to_string(sum8/2) + ", " + uint8_to_string(neg8/3) + "; " + uint16_to_string(diff16/7) + "; " + uint32_to_string(neg32/11) + "; " + uint64_to_string(diff64/15);
      string_println(message/16);
      ();
    }