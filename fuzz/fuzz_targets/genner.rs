#![no_main]
use std::fs;

use libfuzzer_sys::fuzz_target;

use std::io::*;
use sy_smith::*;

fuzz_target!(|data: CompUnit| {
    let path = format!("sy/{}.sy", uuid::Uuid::new_v4());
    let mut file = fs::OpenOptions::new()
        .append(true)
        .create(true)
        .open(&path)
        .unwrap();
    let result = data.to_string();
    file.write_all(result.as_bytes()).unwrap();
    file.write_all(b"\n").unwrap();
});
