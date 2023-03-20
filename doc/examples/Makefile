out/gnatprove/gnatprove.out: pub_sub.gpr generated/rflx.ads src/main.adb
	@gnatprove -P $<

generated/rflx.ads: specs/pub_sub.rflx
	@mkdir -p $(dir $@)
	@rflx generate -d $(dir $@) $<
