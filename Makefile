REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
SUBMODULES = build_utils
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

SERVICE_NAME = dev
UTILS_PATH := build_utils
TEMPLATES_PATH = .

BUILD_IMAGE_TAG := b40627de232e7f04c9abd2c480856dac7bcd9386

CALL_ANYWHERE := all submodules rebar-update compile lint xref test clean distclean

CALL_W_CONTAINER := $(CALL_ANYWHERE)

.PHONY: $(CALL_W_CONTAINER)

all: compile

-include $(UTILS_PATH)/make_lib/utils_container.mk

$(SUBTARGETS): %/.git: %
	git submodule update --init $<
	touch $@

submodules: $(SUBTARGETS)

compile: submodules rebar-update
	$(REBAR) compile

rebar-update:
	$(REBAR) update

test: submodules
	$(REBAR) eunit

xref: submodules
	$(REBAR) as test xref

clean:
	$(REBAR) clean

distclean:
	$(REBAR) clean -a
	rm -rfv _build _builds _cache _steps _temp

lint:
	elvis rock

