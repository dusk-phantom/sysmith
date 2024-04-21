#[inline]
pub fn u82bool(data: &u8) -> bool {
    data % 2 == 0
}

pub fn gen_number(data: &[u8; 5]) -> String {
    let if_int = &data[0];
    let value = [data[1], data[2], data[3], data[4]];
    if u82bool(if_int) {
        let m: i32 = i32::from_be_bytes(value);
        m.to_string()
    } else {
        let m: f32 = f32::from_be_bytes(value);
        m.to_string()
    }
}
