#![no_main]
use std::fs;

use libfuzzer_sys::fuzz_target;

use sy_smith::*;
// #[inline]
// fn u82bool(data: &u8) -> bool {
//     if data % 2 == 0 {
//         true
//     } else {
//         false
//     }
// }

// fn gen_number(data: &[u8; 5]) -> String {
//     let if_int = &data[0];
//     let value = [data[1], data[2], data[3], data[4]];
//     if u82bool(if_int) {
//         let m: i32 = i32::from_be_bytes(value);
//         m.to_string()
//     } else {
//         let m: f32 = f32::from_be_bytes(value);
//         m.to_string()
//     }
// }

fuzz_target!(|data: &[u8]| {
    if data.len() < 5 {
        return;
    }
    // 打开文件sy.txt,往里面附加写入 gen_number
    let mut file = fs::OpenOptions::new().append(true).open("sy.txt").unwrap();
    let mut data_array = [0; 5];
    data_array.copy_from_slice(&data[0..5]);
    use std::io::*;
    let result = gen_number(&data_array);
    file.write_all(result.as_bytes()).unwrap();
    file.write_all(b"\n").unwrap();
});
