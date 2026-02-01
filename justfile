demo:
	cd examples && cabal run

demo-spv:
	cd examples && SPIRDO_WRITE_SPV=1 cabal run
