SHELL         := /bin/sh
BUILD         := bin/build
DEPLOY        := bin/deploy
VERSION       := $(shell awk 'NF{print $$1; exit}' VERSION)

MODULES       := core bus crypto db ip log reitit time validators

moddir         = modules/$1
pomfile        = modules/$1/pom.xml
jarfile        = target/utils-$1-$(VERSION).jar

.PHONY: watch default docs deploy deploy-all \
        test tests test-all \
        pom poms pom-all $(addprefix pom-,$(MODULES)) \
        jar jars jar-all $(addprefix jar-,$(MODULES)) \
        sig sigs sig-all $(addprefix sig-,$(MODULES)) \
        tag clean clean-all

default:		docs

lint:
			bin/lint

docs:
			echo "# Introduction" > doc/10_introduction.md
			tail -n +2 README.md >> doc/10_introduction.md
			bin/docs "$(VERSION)"

push-docs:
			git subtree push --prefix=docs docs master

test-%:
			@rm -rf $(call moddir,$*)/.cpcache
			@bin/test $(call moddir,$*)

tests: $(addprefix test-,$(MODULES))

test: tests

test-all: tests

sync-pom-%:
			@echo "[sync-pom] $*"
			$(BUILD) sync-pom :module :$*

sync-poms: clean $(addprefix sync-pom-,$(MODULES))

sync-pom-all: sync-poms

sync-pom: sync-poms

pom-%: clean
			@echo "[pom] $* -> $(VERSION)"
			@mvn -f $(call pomfile,$*) versions:set versions:commit -DnewVersion="$(VERSION)"
			@mvn -f $(call pomfile,$*) versions:set-scm-tag -DnewTag="$(VERSION)"
			@rm -f $(call pomfile,$*).asc || true
			$(BUILD) sync-pom :module :$*

poms: $(addprefix pom-,$(MODULES))

pom-all: poms

pom: poms

jar-%: clean pom-%
			@echo "[jar] $*"
			$(BUILD) jar :module :$*

jars: $(addprefix jar-,$(MODULES))

jar-all: jars

jar: jars

deploy-%: clean pom-% jar-%
			@echo "[deploy]"
			@test -f "$(call jarfile,$*)" || (echo "Missing $(call jarfile,$*)"; exit 1)
			@test -f "$(call pomfile,$*)" || (echo "Missing $(call pomfile,$*)"; exit 1)
			@echo "[deploy] jar=$(call jarfile,$*)"
			@echo @$(DEPLOY) deploy :artifact "\"$(call jarfile,$*)\""
			@echo @test -f "$(APPNAME)-$(VERSION).pom.asc" && mv -f "$(APPNAME)-$(VERSION).pom.asc" "$(POMFILE).asc" || true

deploy-all: clean-all $(addprefix deploy-,$(MODULES))

deploy: deploy-all

sig-%:
			@echo "[sig] $*"
			@rm -f $(call pomfile,$*).asc
			@gpg2 --armor --detach-sig $(call pomfile,$*)

sigs: $(addprefix sig-,$(MODULES))

sigs-all: sigs

sig: sigs

tag:
			git tag -s "$(VERSION)" -m "Release $(VERSION)"

clean-all: clean
			@rm -f target/*.jar modules/*/pom.xml.asc || true

clean:
			@find . -name .DS_Store -print0 | xargs -0 rm -f
