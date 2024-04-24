#![no_main]
use std::fs;

use libfuzzer_sys::fuzz_target;

use sy_smith::*;

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
