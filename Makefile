.PHONY: all
all: ## Build the code
	jbuilder build --dev

.PHONY: test
test: ## Run unit tests
	jbuilder runtest --force

.PHONY: clean
clean: ## Clean up source tree manually
	jbuilder clean

.PHONY: distrib
distrib: ## Create a distribution tarball
	topkg distrib

.PHONY: tag
tag: ## Tag the current release
	topkg tag

.PHONY: publish
publish: ## Put the release on GitHub
	topkg publish distrib

.PHONY: help
help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
