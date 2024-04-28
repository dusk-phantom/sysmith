.PHONY: run cov show clean

run:
	cargo fuzz run genner

cov: 
	cargo fuzz coverage genner

clean:
	rm sy/*.sy

show:
	cargo cov -- show target/x86_64-unknown-linux-gnu/coverage/x86_64-unknown-linux-gnu/release/genner --format=html -instr-profile=fuzz/coverage/genner/coverage.profdata -ignore-filename-regex='/rustc/.+|.+\.cargo/registry.+' > index.html