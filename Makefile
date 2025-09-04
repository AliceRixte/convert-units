# The following variables can be set in command line. For instance : "make run
# exe=myExe"
pkg = convert-units
trg = lib
exe = $(pkg)-exe
tst = $(pkg)-test
bch = $(pkg)-bench

stack = stack
stack-prof = $(stack) --work-dir $(stack-work-prof) --profile

stack-work = .stack-work
make-work  = .make-work
stack-work-prof = .stack-work-prof
# For explanation about why we need .stack-work-prof, see this stack issue
# https://github.com/commercialhaskell/stack/issues/4032



build:
	$(stack) build $(pkg):$(trg)

run:
	$(stack) build --fast $(pkg):exe:$(exe)  && $(stack) exec $(exe)

targets:
	$(stack) ide targets

clean:
	$(stack) clean

clean-hard:
	$(stack) clean
	rm -r $(make-work)
	rm -r $(stack-work)
	rm -r $(stack-work-prof)

doc :
	$(stack) haddock --no-haddock-deps

bench:
# [[ -d $(make-work) ]] || mkdir $(make-work)
	$(stack) bench $(pkg):bench:$(bch) --ba "--output $(make-work)/$(bch).html"

test:
	$(stack) test $(pkg):test:$(tst) --coverage

install:
	$(stack) install $(pkg)

# Add {-# OPTIONS_GHC -ddump-to-file #-} to Main.hs of the benchmark
ddump:
  $(stack) bench --ghc-options="-ddump-to-file -ddump-simpl" --no-run-benchmarks


# Profiling

prof:
	[[ -d $(make-work) ]] || mkdir $(make-work)
	$(stack-prof) build
	$(stack-prof) exec  -- $(exe) +RTS -p && mv $(exe).EXE.prof ./$(make-work)/$(exe).EXE.prof

prof-test:
	[[ -d $(make-work) ]] || mkdir $(make-work)
	$(stack-prof) test $(pkg):test:$(tst) --test-arguments "+RTS -p" && mv $(tst).prof ./$(make-work)/$(tst).prof

prof-bench:
	[[ -d $(make-work) ]] || mkdir $(make-work)
	$(stack-prof) bench $(pkg):bench:$(bch) --test-arguments "+RTS -p" && mv $(bch).prof ./$(make-work)/$(bch).prof

# GHCi

ghci:
	$(stack) ghci

ghci-exe:
	$(stack) ghci $(pkg):$(trg) --test --main-is $(pkg):$(exe)

ghci-test:
	$(stack) ghci $(pkg):test:$(tst)

ghci-bench:
	$(stack) ghci $(pkg):bench:$(bch)

# GHCid

ide:
	ghcid --command="stack ghci"

ide-exe:
	$(stack) exec -- ghcid -c "stack ghci $(pkg):$(trg) --test --main-is $(pkg):$(exe)"

ide-test:
	$(stack) exec -- ghcid -c "stack ghci $(pkg):test:$(tst) --test "

ide-bench:
	$(stack) exec -- ghcid -c "stack ghci $(pkg):bench:$(bch) --test"


.PHONY : build run targets clean clean-hard bench test prof prof-test prof-bench ghci ghci-exe ghci-test ghci-bench ide ide-exe ide-test ide-bench