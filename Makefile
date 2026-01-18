SHELL       := /bin/sh
BUILD       := bin/build
DEPLOY      := bin/deploy
DOCS        := bin/docs
TEST        := bin/test
LINT        := bin/lint
UPREADME    := bin/update-readme

MODULES     := core bus crypto db ip log reitit time validators

VERSION     ?= 2.0.0
DESCRIPTION ?= Random Utilities for Clojure
GROUP       ?= io.randomseed
APPNAME     ?= utils
POMFILE     := pom.xml
POMVERF     ?= $(APPNAME)-$(VERSION).pom
POMTARG     ?= target/$(APPNAME)-$(VERSION).pom
JARNAME     ?= $(APPNAME)-$(VERSION).jar
JARFILE     ?= target/$(APPNAME)-$(VERSION).jar
CLASSES     ?= target/classes
URL         ?= https://randomseed.io/software/$(APPNAME)/
SCM         ?= github.com/randomseed-io/$(APPNAME)
DOCPREFIX   := $(GROUP)/$(APPNAME)

moddir       = modules/$1
pomfile      = modules/$1/$(POMFILE)
modname      = $(APPNAME)-$1
jarname      = $(APPNAME)-$1-$(VERSION).jar
pomverf      = modules/$1/$(APPNAME)-$1-$(VERSION).pom
pomtarg      = target/$(APPNAME)-$1-$(VERSION).pom
classes      = modules/$1/target/classes
jarfile      = target/$(APPNAME)-$1-$(VERSION).jar
escape_dq    = $(subst \,\\,$(subst ",\",$1))
firstline    = $(strip $(shell sed -n '1p' "$1" 2>/dev/null))
moddesc      = $(call escape_dq,$(if $(wildcard modules/$1/DESCRIPTION),$(call firstline,modules/$1/DESCRIPTION),$(DESCRIPTION)))
modsrcdirs   = $(addsuffix /src,$(addprefix modules/,$(MODULES)))
modsrcdirse  = [$(foreach p,$(modsrcdirs),"$(p)" )]

.PHONY: watch default docs
.PHONY: deploy deploys deploy-all deploy-meta
.PHONY: test tests test-all
.PHONY: sync-pom sync-poms sync-pom-all sync-meta-pom
.PHONY: pom poms pom-all meta-pom
.PHONY: jar jars jar-all meta-jar
.PHONY: sig sigs sig-all meta-sig
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

sync-meta-pom:
	@echo "[sync-pom]      -> $(POMFILE)"
	@$(BUILD) sync-pom                  \
	  :group       "\"$(GROUP)\""       \
	  :name        "\"$(APPNAME)\""     \
	  :version     "\"$(VERSION)\""     \
	  :description "\"$(DESCRIPTION)\"" \
	  :scm         "\"$(SCM)\""         \
	  :url         "\"$(URL)\""         \
	  :aliases     '[:mono]'

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

sync-poms: clean $(MODULES:%=sync-pom-%) sync-meta-pom

sync-pom-all: sync-poms

sync-pom: sync-poms

meta-pom:
	@echo "[pom]      -> $(POMFILE)"
	@rm -f $(POMFILE).asc || true
	@$(BUILD) sync-pom                  \
	  :group       "\"$(GROUP)\""       \
	  :name        "\"$(APPNAME)\""     \
	  :version     "\"$(VERSION)\""     \
	  :description "\"$(DESCRIPTION)\"" \
	  :scm         "\"$(SCM)\""         \
	  :url         "\"$(URL)\""         \
	  :aliases     '[:mono]'

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

poms: clean $(MODULES:%=pom-%) meta-pom

pom-all: poms

pom: poms

meta-jar: meta-pom
	@echo "[jar]      -> $(JARNAME)"
	@rm -rf $(CLASSES) $(JARFILE) || true
	@$(BUILD) jar               \
	  :group   "\"$(GROUP)\""   \
	  :name    "\"$(APPNAME)\"" \
	  :version "\"$(VERSION)\"" \
	  :aliases '[:mono]'

jar-%: pom-%
	@echo "[jar]      -> $(call jarname,$*)"
	@rm -rf $(CLASSES) $(call jarfile,$*) || true
	@$(BUILD) jar                       \
	  :module  :$*                      \
	  :group   "\"$(GROUP)\""           \
	  :name    "\"$(call modname,$*)\"" \
	  :version "\"$(VERSION)\""

jars: clean-all poms $(MODULES:%=jar-%) meta-jar

jar-all: jars

jar: jars

release: tests clean-all docs jars

meta-deploy: clean meta-pom meta-jar
	@echo "[deploy]   -> $(GROUP)/$(APPNAME)-$(VERSION)"
	@test -f "$(JARFILE)" || (echo "Missing $(JARFILE)"; exit 1)
	@test -f "$(POMFILE)" || (echo "Missing $(POMFILE)"; exit 1)
	echo @$(DEPLOY) deploy :artifact "\"$(JARFILE)\""
	@test -f "$(POMVERF).asc" && mv -f "$(POMVERF).asc" "$(POMFILE).asc" || true
	@test -f "$(POMTARG).asc" && mv -f "$(POMTARG).asc" "$(POMFILE).asc" || true

deploy-%: clean pom-% jar-%
	@echo "[deploy]   -> $(GROUP)/$(call modname,$*)-$(VERSION)"
	@test -f "$(call jarfile,$*)" || (echo "Missing $(call jarfile,$*)"; exit 1)
	@test -f "$(call pomfile,$*)" || (echo "Missing $(call pomfile,$*)"; exit 1)
	echo @$(DEPLOY) deploy :artifact "\"$(call jarfile,$*)\""
	@test -f "$(call pomverf,$*).asc" && mv -f "$(call pomverf,$*).asc" "$(call pomfile,$*).asc" || true
	@test -f "$(call pomtarg,$*).asc" && mv -f "$(call pomtarg,$*).asc" "$(call pomfile,$*).asc" || true

deploy-all: clean-all jars $(MODULES:%=deploy-%) meta-deploy

deploy: deploy-all

meta-sig:
	@echo "[sig]      -> $(POMFILE).asc"
	@rm -f $(POMFILE).asc || true
	@gpg2 --armor --detach-sig $(POMFILE)

sig-%:
	@echo "[sig]      -> $(call pomfile,$*).asc"
	@rm -f $(call pomfile,$*).asc || true
	@gpg2 --armor --detach-sig $(call pomfile,$*)

sigs: $(MODULES:%=sig-%) meta-sig

sigs-all: sigs

sig: sigs

tag:
	git tag -s "$(VERSION)" -m "Release $(VERSION)"

clean-all: clean
	@rm -rf target/classes || true
	@rm -f target/*.jar modules/*/pom.xml.asc "$(POMFILE).asc" || true

clean:
	@find . -name .DS_Store -print0 | xargs -0 rm -f
