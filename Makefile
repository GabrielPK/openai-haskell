# cabal needs the package name as defined in openai-haskell.cabal
package = openai-haskell

# target ?= $(package):lib:test-dev
target ?= $(package):lib

ifdef warn
GHC_OPTIONS ?=
else
GHC_OPTIONS ?= -Werror
endif

# Colors for displaying text in the terminal
RED    := $(shell tput -Txterm setaf 1)
GREEN  := $(shell tput -Txterm setaf 2)
YELLOW := $(shell tput -Txterm setaf 3)
WHITE  := $(shell tput -Txterm setaf 7)
BOLD   := $(shell tput -Txterm bold)
RESET  := $(shell tput -Txterm sgr0)

GHCI_FLAGS=--repl-options="-j -fwrite-interface -fno-defer-diagnostics -ignore-dot-ghci"

GHCID_OPTIONS = $(GHCI_FLAGS) --repl-options="-fno-break-on-exception -fno-break-on-error -v1 -ferror-spans"

################################################################################
# env setup 
################################################################################

# ensure in nix shell 
check-in-nix-shell:
ifndef IN_NIX_SHELL
	$(error $(BOLD)$(RED)Enter Nix shell to continue$(RESET))
endif
.PHONY: check-in-nix-shell

################################################################################
# building
################################################################################

all: check-in-nix-shell
	build

build: check-in-nix-shell
	cabal build $(target)

clean: check-in-nix-shell
	cabal clean

ghcid: check-in-nix-shell
	ghcid --command "cabal repl $(target) $(GHCID_OPTIONS)"