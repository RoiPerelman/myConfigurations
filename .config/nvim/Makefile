NVIM_EXEC ?= nvim

all: test

test:
	for nvim_exec in $(NVIM_EXEC); do \
		printf "\n======\n\n" ; \
		$$nvim_exec --version | head -n 1 && echo '' ; \
		$$nvim_exec --headless --noplugin -u ./scripts/minimal_init.lua \
			-c "lua MiniTest.run()" ; \
	done

# Use `make test_xxx` to run tests for module 'mini.xxx'
TEST_MODULES = $(basename $(notdir $(wildcard tests/test_*.lua)))

$(TEST_MODULES):
	for nvim_exec in $(NVIM_EXEC); do \
		printf "\n======\n\n" ; \
		$$nvim_exec --version | head -n 1 && echo '' ; \
		$$nvim_exec --headless --noplugin -u ./scripts/minimal_init.lua \
			-c "lua MiniTest.run_file('tests/$@.lua')" ; \
	done
