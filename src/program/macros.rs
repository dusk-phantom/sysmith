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

/// Select from feasible choices
#[macro_export]
macro_rules! from_feasible {
    ($size:expr, $u:expr; $c:expr; $($ty:expr),+) => {
        let flag = [$($ty::can_arbitrary($c),)*]
            .into_iter()
            .map(|b| if b { 1 } else { 0 });
        let total = flag.clone().reduce(|acc, e| acc + e).unwrap();
        let mut rand = u.int_in_range(0..=total - 1)?;
        let mut selected = 0;

        // Get selected index. For unable types,
        // as `rand` is not reduced, break will not be possible
        for i in flag {
            rand -= i;
            if rand < 0 {
                break;
            }
            selected += 1;
        }

        // Select branch, lazy load
        let action = [$((|| $ty::arbitrary($u, $c)?.into()),)*];
        action[selected]()
    };
}
