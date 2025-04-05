.PHONY: clean
clean:
	rm -rf dist-newstyle
	rm -rf verilog
	rm -rf netlist

format:
	stylish-haskell -c ./.stylish-haskell.yaml -r -i src tests

check_format:
	cp ./.stylish-haskell.yaml ./.stylish-haskell-check.yaml
	echo "exit_code: error_on_format" >> ./.stylish-haskell-check.yaml
	stylish-haskell -c ./.stylish-haskell-check.yaml -r src tests

clean_tests:
	rm ./.stylish-haskell-check.yaml

test:
	cabal test

/run/netns/colorlight:
	sudo ip netns add colorlight
	sudo ip link set $(IFNAME) netns colorlight
	sudo ip netns exec colorlight ip addr add 192.168.1.0/24 dev $(IFNAME)
	sudo ip netns exec colorlight ip6tables -A OUTPUT -p icmpv6 --icmpv6-type router-solicitation -j DROP
	sudo ip netns exec colorlight ip link set $(IFNAME) up

.PHONY: namespace
namespace: /run/netns/colorlight

.PHONY: delete_namespace
delete_namespace:
	@if [ -f /run/netns/colorlight ]; then \
		echo "Deleting namespace colorlight"; \
		sudo ip netns delete colorlight; \
	else \
		echo "Namespace colorlight does not exist"; \
	fi

.PHONY: python_test
python_test: /run/netns/colorlight prog
	sudo ip netns exec colorlight \
	sudo "IFNAME=$(IFNAME)" "DEV=$(DEV)" "PATH=$$PATH" "PYTHONPATH=$$PYTHONPATH" python3 -m unittest discover -s python_tests

HASKELL_SOURCES=$(shell find src -type f -iname '*.hs')

COPY = cp -a

verilog=verilog/Clash.Lattice.ECP5.ButterStick.TopEntity.topEntity/topEntity.v
netlist=netlist/synth.json
pnr=netlist/pnr.cfg
bitstream=netlist/clash-eth.bit
dfu=netlist/clash-eth.dfu
${verilog}: ${HASKELL_SOURCES}
	cabal run clash -- Clash.Lattice.ECP5.ButterStick.TopEntity --verilog -g -fclash-clear

.PHONY: verilog
verilog: $(verilog)

${netlist}: ${verilog}
	mkdir -p netlist
	yosys \
		-m ${YOSYS_ECP5_INFER_OUTREG_LIB} \
		-p "synth_ecp5 -no-rw-check -abc2 -top topEntity" \
		-p "ecp5_infer_bram_outreg" \
		-p "write_json ${netlist}" \
		verilog/Clash.Lattice.ECP5.ButterStick.TopEntity.topEntity/*.v

.PHONY: netlist
netlist: $(netlist)

${pnr}: ${netlist} pinout.lpf
	nextpnr-ecp5 --json ${netlist} \
		--lpf pinout.lpf \
		--textcfg ${pnr} --um5g-85k \
		--speed 8 \
		--package CABGA381 \
		--no-tmdriv \
		--randomize-seed --timing-allow-fail

.PHONY: pnr
pnr: $(pnr)

${bitstream}: ${pnr}
	ecppack --compress --freq 38.8 --input ${pnr} --bit ${bitstream}

.PHONY: bitstream
bitstream: $(bitstream)

%.dfu : %.bit
	$(COPY) $< $@
	dfu-suffix -v 1209 -p 5af1 -a $@

dfu: ${dfu}
	dfu-util --alt 0 --download $< --reset

prog: ${bitstream}
	sudo "PATH=$$PATH" env ecpprog -S ${bitstream}

flash: ${bitstream}
	sudo "PATH=$$PATH" env ecpprog -p -a ${bitstream}

int:
	cabal run -- clashi

hoogle:
	hoogle server --local --port 8080
