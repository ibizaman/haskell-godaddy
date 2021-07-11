.PHONY: aur aur-push aur-make build build-release test run hoogle-build hoogle-generate hoogle-serve cachix-enable cachix-push

haskell-godaddy.cabal: package.yaml
	hpack package.yaml

build:
	stack --nix build

build-release-lib:
	nix-build -A haskell-godaddy.components.library

build-release-exe:
	nix-build -A haskell-godaddy.components.exes
	echo "Executable is in ./result/bin"

build-release:
	nix-build -A haskell-godaddy.components.exes
	cp result/bin/haskell-godaddy-exe godaddy
	chmod 755 godaddy
	patchelf --set-interpreter /lib/ld-linux-x86-64.so.2 godaddy
	patchelf --set-rpath /lib godaddy

test:
	stack --nix test

hoogle-build:
	stack --nix build --haddock --haddock-deps

hoogle-generate:
	stack --nix hoogle -- generate --quiet --local

hoogle-serve:
	stack --nix hoogle -- server --local --port=65000 --no-security-headers

cachix-enable:
	cachix use ibizaman

cachix-push:
	nix-build -A haskell-godaddy.components.library | cachix push ibizaman
	nix-build -A haskell-godaddy.components.exes | cachix push ibizaman
	nix-build shell.nix | cachix push ibizaman

hackage-prepare: haskell-godaddy.cabal
	nix-shell --run "cabal check" || exit 1
	stack sdist --tar-dir . --tar-dir .


aur:
	cd aur && ./update-aur.sh

aur-push:
	git submodule foreach 'git push'

aur-make:
	cd aur && rm godaddy.zip && makepkg -f
