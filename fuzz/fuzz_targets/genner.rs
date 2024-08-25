#![no_main]
// Copyright 2024 Duskphantom Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// SPDX-License-Identifier: Apache-2.0

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
