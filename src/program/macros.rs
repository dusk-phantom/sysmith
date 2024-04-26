/// Randomly retry all cases until one of them succeeds.
/// Safety: If the last case can fail, the error should be handled manually.
#[macro_export]
macro_rules! random_retry {
    (size = $total:expr, bytes = $source:expr; $($pat:pat => $expr:expr),+ $(,)?) => {
        {
            let total: u8 = $total;
            let mut remain: u8 = total;
            loop {
                if $source.int_in_range(0..=remain-1)? == 0 {
                    let result = match total - remain {
                        $($pat => $expr,)*
                        _ => unreachable!(),
                    };
                    match result {
                        Ok(x) => return Ok(x),
                        Err(e) => {
                            if remain > 1 {
                                remain -= 1;
                            } else {
                                return Err(e);
                            }
                        },
                    }
                }
            }
        }
    };
}