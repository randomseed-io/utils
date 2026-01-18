SHELL       := /bin/sh
BUILD       := bin/build
DEPLOY      := bin/deploy
DOCS        := bin/docs
TEST        := bin/test
LINT        := bin/lint
UPREADME    := bin/update-readme

MODULES     := core bus crypto db ip log reitit time validators

VERSION     ?= 2.0.0
GROUP       ?= io.randomseed
APPNAME     ?= utils
DESCRIPTION ?= Random Utilities for Clojure
URL         ?= https://randomseed.io/software/$(APPNAME)/
SCM         ?= github.com/randomseed-io/$(APPNAME)

POMFILE     := pom.xml
DOCPREFIX   := $(GROUP)/$(APPNAME)

moddir       = modules/$1
pomfile      = modules/$1/$(POMFILE)
modname      = $(APPNAME)-$1
jarname      = $(APPNAME)-$1-$(VERSION).jar
pomtarg      = target/utils-$1-$(VERSION).pom
classes      = modules/$1/target/classes
jarfile      = target/$(APPNAME)-$1-$(VERSION).jar
modpomf      = target/utils-$1-$(VERSION).pom
escape_dq    = $(subst \,\\,$(subst ",\",$1))
firstline    = $(strip $(shell sed -n '1p' "$1" 2>/dev/null))
moddesc      = $(call escape_dq,$(if $(wildcard modules/$1/DESCRIPTION),$(call firstline,modules/$1/DESCRIPTION),$(DESCRIPTION)))
modsrcdirs   = $(addsuffix /src,$(addprefix modules/,$(MODULES)))
modsrcdirse  = [$(foreach p,$(modsrcdirs),"$(p)" )]

.PHONY: watch default docs
.PHONY: deploy deploys deploy-all
.PHONY: test tests test-al
.PHONY: sync-pom sync-poms sync-pom-all
.PHONY: pom poms pom-all
.PHONY: jar jars jar-all
.PHONY: sig sigs sig-all
.PHONY: tag clean clean-all

default: docs

lint:
	@$(LINT)

readme:
	@echo "[readme]    -> README.md"
	@$(UPREADME) "$(DOCPREFIX)" "$(VERSION)" README.md

docs: readme
	@echo "[docs]      -> docs/"
	@echo "# Introduction" > doc/10_introduction.md
	@tail -n +2 README.md >> doc/10_introduction.md
	@$(DOCS) :version '"$(VERSION)"' :src-dirs '$(modsrcdirse)'

push-docs:
	git subtree push --prefix=docs docs master

test-%:
	@rm -rf $(call moddir,$*)/.cpcache || true
	@$(TEST) $(call moddir,$*)

tests: $(MODULES:%=test-%)

test: tests

test-all: tests

sync-pom-%:
	@echo "[sync-pom] -> $(call pomfile,$*)"
	@$(BUILD) sync-pom                      \
	  :module      :$*                      \
	  :group       "\"$(GROUP)\""           \
	  :name        "\"$(call modname,$*)\"" \
	  :version     "\"$(VERSION)\""         \
	  :description "\"$(call moddesc,$*)\"" \
	  :scm         "\"$(SCM)\""             \
	  :url         "\"$(URL)\""

sync-poms: clean $(MODULES:%=sync-pom-%)

sync-pom-all: sync-poms

sync-pom: sync-poms

pom-%:
	@echo "[pom]      -> $(call pomfile,$*)"
	@rm -f $(call pomfile,$*).asc || true
	@$(BUILD) sync-pom                      \
	  :module      :$*                      \
	  :group       "\"$(GROUP)\""           \
	  :name        "\"$(call modname,$*)\"" \
	  :version     "\"$(VERSION)\""         \
	  :description "\"$(call moddesc,$*)\"" \
	  :scm         "\"$(SCM)\""             \
	  :url         "\"$(URL)\""

poms: clean $(MODULES:%=pom-%)

pom-all: poms

pom: poms

jar-%: pom-%
	@echo "[jar]      -> $(call jarname,$*)"
	@rm -rf $(call classes,$*) $(call jarfile,$*) || true
	@$(BUILD) jar                               \
	          :module  :$*                      \
	          :group   "\"$(GROUP)\""           \
	          :name    "\"$(call modname,$*)\"" \
	          :version "\"$(VERSION)\""

jars: clean-all poms $(MODULES:%=jar-%)

jar-all: jars

jar: jars

release: tests clean-all docs jars

deploy-%: clean pom-% jar-%
	@echo "[deploy]   -> $(GROUP)/$(call modname,$*)-$(VERSION)"
	@test -f "$(call jarfile,$*)" || (echo "Missing $(call jarfile,$*)"; exit 1)
	@test -f "$(call pomfile,$*)" || (echo "Missing $(call pomfile,$*)"; exit 1)
	@$(DEPLOY) deploy :artifact "\"$(call jarfile,$*)\""
	@test -f "$(call modpomf,$*).asc" && mv -f "$(call modpomf,$*).asc" "$(call pomfile,$*).asc" || true
	@test -f "$(call modtarg,$*).asc" && mv -f "$(call modtarg,$*).asc" "$(call pomfile,$*).asc" || true

deploy-all: clean-all jars $(MODULES:%=deploy-%)

deploy: deploy-all

sig-%:
	@echo "[sig]      -> $(POMFILE).asc"
	@rm -f $(call pomfile,$*).asc || true
	@gpg2 --armor --detach-sig $(call pomfile,$*)

sigs: $(MODULES:%=sig-%)

sigs-all: sigs

sig: sigs

tag:
	git tag -s "$(VERSION)" -m "Release $(VERSION)"

clean-all: clean
	@rm -f target/*.jar target/classes modules/*/pom.xml.asc "$(POMFILE).asc" || true

clean:
	@find . -name .DS_Store -print0 | xargs -0 rm -f
