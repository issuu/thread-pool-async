.PHONY: all
all: ## Build the code
	dune build

.PHONY: test
test: ## Run unit tests
	dune runtest --force

.PHONY: clean
clean: ## Clean up source tree manually
	dune clean

.PHONY: distrib
distrib: ## Create a distribution tarball
	dune-release distrib

.PHONY: tag
tag: ## Tag the current release
	dune-release tag

.PHONY: publish
publish: ## Put the release on GitHub
	dune-release publish distrib

.PHONY: help
help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
