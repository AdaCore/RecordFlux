PROJECT_FILE = pub_sub.gpr
SPECS = $(wildcard specs/*.rflx)
GENERATED = build/generated/rflx.ads $(wildcard build/generated/*.{adb,ads})
SRC = $(wildcard src/*.{adb,ads})
BIN = build/obj/main

.PHONY: build prove clean

build: $(BIN)

prove: $(PROJECT_FILE) $(GENERATED) $(SRC)
	@gnatprove -P $<

clean:
	@rm -rf build

$(BIN): $(PROJECT_FILE) $(GENERATED) $(SRC)
	@gprbuild -P $<

$(GENERATED): $(SPECS)
	@mkdir -p $(dir $@)
	@rflx generate -d $(dir $@) $<
