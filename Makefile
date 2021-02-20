.PHONY: build test run hoogle-build hoogle-generate hoogle-serve cachix-enable cachix-push

build:
	stack --nix build

test:
	stack --nix test

run:
	stack --nix run

hoogle-build:
	stack --nix build --haddock --haddock-deps

hoogle-generate:
	stack --nix hoogle -- generate --quiet --local

hoogle-serve:
	stack --nix hoogle -- server --local --port=65000 --no-security-headers

cachix-enable:
	cachix use ibizaman

cachix-push:
	nix-build | cachix push ibizaman
	nix-build shell.nix | cachix push ibizaman
